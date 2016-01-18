#!/usr/bin/env bash
cd ../server
until stack exec alphasheets-exe; do
    if [ $? -eq 137 ]
        then exit
    fi
    # Given a message to post as an argument, will post that message to slack general
    webhook_url="https://hooks.slack.com/services/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"
    channel="#builds"
    username="DeployBot"
    text="Backend on $1 has exited. Respawning!"
    escapedText=$(echo $text | sed 's/"/\"/g' | sed "s/'/\'/g" )
    json="{\"channel\": \"$channel\", \"username\":\"$username\", \"icon_emoji\":\"ghost\", \"attachments\":[{\"color\":\"danger\" , \"text\": \"$escapedText\"}]}"
    # The -k is sort of a hack, couldn't get certificate stuff to work out
    curl -k -d "payload=$json" "$webhook_url"
    echo "Backend server crashed with exit code $?. Respawning..." >&2
    sleep 1
done