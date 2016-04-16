from functools import wraps
import websocket
import json
import sys
import errno
import os
import signal
import subprocess
import shortid
from time import gmtime, strftime

# This script will try to evaluate in all languages. If it fails in any way, it will trigger a 
# redeploy. It's meant to be run from the root directory, with two arguments: 
# 1) either "stable" or "master"
# 2) The websocket address (usually ws://localhost:5000)

#---------------------------------------------------------------------------------------------------
# Constants

languages = ["Excel", "Python", "R"]
server_type = sys.argv[1]
address = sys.argv[2]

sid = shortid.ShortId();

def id_gen():
  return sid.generate()

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
    "tag": "Login", 
    "contents": {
      "tag": "TestAuth",
      "contents": []
    }
  }
  return json.dumps(msg)

def clear_message(sheetId):
  msg = {
    "serverAction":{
      "tag": "ClearSheetServer",
      "contents": sheetId
    },
    "messageId": id_gen()
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
    "messageId": id_gen()
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

# Return false or the ith element of a list
def get_bool(lst, i):
  try:
    return lst[i]
  except Exception as e:
    return False

# On a failure, just notify Slack and bring up relevant microservices
def handle_failure(test_successes):

  if server_type == "stable":
    if not get_bool(test_successes, 0):
      subprocess.call(["curl -s http://stable.alphasheets.com:10000/job/stable-deploy/build"], shell = True)
      return
    if not get_bool(test_successes, 1):
      subprocess.call(['tmux kill-session -t demo_python_kernel'], shell = True)
      subprocess.call(["cd backend/pykernel; tmux new -s demo_python_kernel -d 'python server.py'"], shell = True)
    if not get_bool(test_successes, 2):
      subprocess.call(['tmux kill-session -t demo_rkernel'], shell = True)
      subprocess.call(['cd scripts; tmux new -s demo_rkernel -d ./start_rkernel.sh'], shell = True)
  
  elif server_type == "master":
    if not get_bool(test_successes, 0):
      subprocess.call(["curl -s http://builds.alphasheets.com/job/master-deploy/build"], shell = True)
      return 
    if not get_bool(test_successes, 1):
      subprocess.call(["tmux kill-session -t pykernel"], shell = True)
      subprocess.call(["cd backend/pykernel; tmux new -s pykernel -d 'export PATH=\"/root/anaconda2/bin:$PATH\"; ./server.sh'"], shell = True)
    if not get_bool(test_successes, 2):
      subprocess.call(["tmux kill-session -t rkernel"], shell = True)
      subprocess.call(["cd backend/server; tmux new -s rkernel -d './rkernel-exe'"], shell = True)


  pwt("Handling failure")
  slack_msg1 = "\"mayday mayday " + server_type + " down. "
  slack_msg2 = "Excel test passed: " + str(get_bool(test_successes, 0)) + ". "
  slack_msg3 = "Python test passed: " + str(get_bool(test_successes, 1)) + ". "
  slack_msg4 = "R test passed: " + str(get_bool(test_successes, 2)) + ".\""
  slack_msg = slack_msg1 + slack_msg2 + slack_msg3 + slack_msg4
  slack_cmd = "bash send-slack.sh " + slack_msg + " #general plumbus-bot"
  subprocess.call(["cd scripts; " + slack_cmd], shell = True)

def handle_ws_down():
  slack_msg = "\"Websocket connection to " +  server_type + " may have failed.\""
  slack_cmd = "bash send-slack.sh " + slack_msg + " #general plumbus-bot"
  subprocess.call([slack_cmd], shell = True)

#---------------------------------------------------------------------------------------------------
# Printing with time

def pwt(s):
  time = strftime("%Y-%m-%d %H:%M:%S") 
  msg = "[" + time + "] " + s 
  print msg

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
  print ("\t Response: " + result)
  return is_correct_eval_result(result)

@timeout(50, os.strerror(errno.ETIMEDOUT))
def test():
  try:
    print "================================================================"
    ws = websocket.create_connection(address)
    pwt("Created ws connection")
    ws.send(login_message())
    pwt("Sent login message")
    sheetId = get_sheet_id(get_response(ws))
    pwt("Got sheet id: " + sheetId)
    test_successes = []
    for lang in languages:
      try:
        success = test_lang(ws, lang, sheetId)
      except Exception as e:
        success = False
      pwt("Test for " + lang + " was success? " + str(success))
      test_successes.append(success)
    if not all(test_successes):
      handle_failure(test_successes)
    pwt("About to close ws in try block")
    ws.close()
    # It seems like this isn't synchronous, so running a bunch of these quickly in succession may
    # not work as expected.
    pwt("Closed ws in try block")
  except Exception as e:
    try:
      pwt("Exception: " + str(e))
      handle_failure(test_successes)
      pwt("Handled failure after exception")
      pwt("About to close ws in except block")
      ws.close()
      pwt("Closed ws in except block")
    except Exception as e:
      pwt("Error: " + str(e))
      handle_ws_down()

test()
