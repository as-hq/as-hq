{-# LANGUAGE OverloadedStrings #-}
module AS.Config.Constants where 

import AS.Prelude
import Data.Text hiding (unlines)

-- daemons do not require generated MessageIDs because they are 
-- send-only clients. They don't receive any data back from the 
-- server. Thus they have no concept of progress or state, and we 
-- can fill in a dummy id until a different type is created for 
-- DaemonMessage
daemon_message_id :: Text
daemon_message_id = "DAEMON_MESSAGE_ID"

-- the response to an invalid initialization message has no origin 
-- ID, since the message was invalid. Therefore, we fill in a dummy ID
-- until a different message type is created for InitializationMessage.
initialization_failure_message_id :: Text
initialization_failure_message_id = "FAILURE_MESSAGE_ID"

-- currently, binary importing is handled simply by sending binary frames
-- across websockets. These messages are raw data and don't have a notion
-- of a MessageID currently. When file importing is handled by an actual 
-- HTTP server in the future, this will be unnecessary since we'll have a 
-- new type for ImportMessage. 
import_message_id :: Text
import_message_id = "IMPORT_MESSAGE_ID"

-- timeout messages are not associated with any messageId sent from the 
-- client, and therefore have no concept of progress. 
timeout_message_id :: Text
timeout_message_id = "TIMEOUT_MESSAGE_ID"

initial_open_message_id :: Text
initial_open_message_id = "INITIAL_OPEN_MESSAGE_ID"

initialize_message_id :: Text
initialize_message_id = "INITIALIZE_MESSAGE_ID"

auth_message_id :: Text
auth_message_id = "auth_message_id"

test_message_id :: Text
test_message_id = "TEST_ID"

new_sheet_name :: String
new_sheet_name = "Sheet1"

-- Currently used only in the date parser. Eric Miller expected 3/5 to autoexpand to 3/5/2016, so 
-- implementing this should make him pretty happy. Unfortunately, programatically getting
-- the current year means you have to parse in the IO monad, which seems like opening a can of worms, and
-- is totally not worth the trouble. PLUS, EVEN IF THIS CODE DOESN'T GET BULLDOZED WITHIN 8 MONTHS AND 19
-- DAYS, THIS IS A VERY OBVIOUS AND TRIVIAL BUG TO FIX. #AlexSolution (yes yes Michael blah blah blah)
current_year :: Integer
current_year = 2016

pythonHeaderDefaultText :: String
pythonHeaderDefaultText = unlines [
  "# This is the Python global code editor. ", 
  "# Any functions and variables defined here ", 
  "# can be referenced within any Python cell. ",
  "# ",
  "# Try uncommenting the below lines (select", 
  "# lines 8-11 and press Ctrl+/):",
  "#",  
  "# def foo(x):",
  "#   return x ** 2",
  "# ",
  "# bar = 3",
  "# ",
  "# Then press the \"Evaluate\" button at the",
  "# top of this editor, and evaluate",
  "# foo(bar) in some cell with the language",
  "# set to Python. ",
  "# ",
  "# You can also see code outputs by ",
  "# evaluating the header. Uncomment the ",
  "# below lines and press evaluate: ",
  "# ",
  "# print \"Printed text!\"",
  "# 3 ** 2 + 1" ]

rHeaderDefaultText :: String
rHeaderDefaultText = unlines [ 
  "# This is the R global code editor. Any ",
  "# functions and variables defined here can ",
  "# be referenced within any R cell. ",
  "# ",
  "# Try uncommenting the below lines (select",
  "# lines 8-12 and press Ctrl+/):",
  "# ",
  "# foo <- function(x) {",
  "#   return(x**2)",
  "# }",
  "# ",
  "# bar <- 4",
  "# ",
  "# Then press the \"Evaluate\" button at the",
  "# top of this editor, and evaluate",
  "# foo(bar) in some cell with the language",
  "# set to R. " ]