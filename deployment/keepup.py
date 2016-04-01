from functools import wraps
import websocket
import json
import sys
import errno
import os
import signal
import subprocess

# This script will try to evaluate in all languages. If it fails in any way, it will trigger a 
# redeploy. It's meant to be run from the root directory, with two arguments: 
# 1) either "stable" or "master"
# 2) The backend port (usually 5000)

#---------------------------------------------------------------------------------------------------
# Constants

languages = ["Excel", "Python", "R"]
server_type = sys.argv[1]
port = sys.argv[2]
address = "ws://localhost:" + str(port)

#---------------------------------------------------------------------------------------------------
# Timeout errors

class TimeoutError(Exception):
    pass

def timeout(seconds=10, error_message=os.strerror(errno.ETIME)):
    def decorator(func):
        def _handle_timeout(signum, frame):
            raise TimeoutError(error_message)
        def wrapper(*args, **kwargs):
            signal.signal(signal.SIGALRM, _handle_timeout)
            signal.alarm(seconds)
            try:
                result = func(*args, **kwargs)
            finally:
                signal.alarm(0)
            return result
        return wraps(func)(wrapper)
    return decorator

#---------------------------------------------------------------------------------------------------
# Message creation

def login_message():
  msg = {
    "tag": "TestAuth",
    "contents": []
  }
  return json.dumps(msg)

def clear_message(sheetId):
  msg = {
    "serverAction":{
      "tag": "ClearSheetServer",
      "contents": sheetId
    },
    "messageId": "41z040DRx"
  }
  return json.dumps(msg)

def eval_message(language, sheetId):
  msg = {
    "serverAction": {
      "tag": "Evaluate",
      "contents": [{
        "tag":" EvalInstruction",
        "evalXp": {
          "expression": "=1+1",
          "language": language,
        },
        "evalLoc":{
          "tag": "index",
          "index": {
            "row": 1,
            "col": 1
          },
          "sheetId": sheetId
        }
      }]
    },
    "messageId":"4kE3UXCDRg"
  }
  return json.dumps(msg)

#---------------------------------------------------------------------------------------------------
# Helpers for analyzing responses

def get_sheet_id(msg):
 return json.loads(msg)["clientAction"]["defaultSheetId"]

def is_correct_eval_result(msg):
  try:
    res = json.loads(msg)
    correctTag = res["clientAction"]["tag"] == "UpdateSheet"
    newValue = res["clientAction"]["contents"]["cellUpdates"]["newVals"][0]["cellValue"]
    isCorrect = (newValue["tag"] == "ValueI") and (newValue["contents"] == 2)
    return isCorrect
  except Exception as e:
    return False

# Wait for a non-ping and non-timeout
def get_response(ws):
  result = ws.recv()
  if result == "PING":
    return get_response(ws)
  elif json.loads(result)["clientAction"]["tag"] == "AskTimeout":
    return get_response(ws)
  else:
    return result

#---------------------------------------------------------------------------------------------------
# Handle failure

def handle_failure():
  if server_type == "stable":
    subprocess.call(
      ["curl -s http://stable.alphasheets.com:10000/job/stable-deploy/build"], 
      shell = True)
  if server_type == "master":
    subprocess.call(
      ["curl -s http://builds.alphasheets.com/job/master-deploy/build"],
      shell = True)
  slack_msg = "\"mayday mayday " + server_type + " down\""
  slack_cmd = "bash send-slack.sh " + slack_msg + " #general plumbus-bot"
  subprocess.call(["cd scripts"], shell = True)
  subprocess.call([slack_cmd], shell = True)

#---------------------------------------------------------------------------------------------------
# Main communication/testing methods

@timeout(10, os.strerror(errno.ETIMEDOUT))
def clear(ws, sheetId):
  ws.send(clear_message(sheetId))
  get_response(ws)

@timeout(10, os.strerror(errno.ETIMEDOUT))
def test_lang(ws, lang, sheetId):
  ws.send(eval_message(lang, sheetId))
  result =  get_response(ws)
  return is_correct_eval_result(result)

@timeout(50, os.strerror(errno.ETIMEDOUT))
def test():
  try:
    ws = websocket.create_connection(address)
    ws.send(login_message())
    sheetId = get_sheet_id(get_response(ws))
    print ("Got sheet id: " + sheetId)
    test_success = True
    for lang in languages:
      success = test_lang(ws, lang, sheetId)
      print ("Test for " + lang + " was success? " + str(success))
      if not success:
        test_success = False
        break
      clear(ws, sheetId)
    if not test_success:
      handle_failure()
    ws.close()
  except Exception as e:
    print e
    handle_failure()
    try:
      ws.close()
    except Exception as e:
      pass

test()
