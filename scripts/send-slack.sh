#!/bin/bash

# Will post a message to AlphaSheets Slack
# First arg: message, second arg: channel (#general, @riteshr), third arg: bot name 

WEBHOOK_URL="https://hooks.slack.com/services/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"
CHANNEL=${2:-"#general"}
USERNAME=${3:-"as-bot"}
TEXT=${1:-"Donald Trump wants to bang his daughter"}

# Note that we don't currently do escaping, since we want newlines to show up
# Note that there are some weird character transforms that slack expects with &, @ etc that we're not taking care of right now

JSON="{\"channel\": \"$CHANNEL\", \"username\":\"$USERNAME\", \"icon_emoji\":\"ghost\", \"attachments\":[{\"color\":\"#764FA5\" , \"text\": \"$TEXT\", \"mrkdwn_in\": [\"text\"]}]}"

# The -k is sort of a hack, couldn't get certificate stuff to work out
curl -k -d "payload=$JSON" "$WEBHOOK_URL"

