#!/bin/bash

unlockKeychain() {
	KCP=$1
	if [ "" = "$KCP" ]; then
	 	echo "No password specified for keychain unlock - aborting"
		exit 1
	fi

	KC="$HOME/Library/Keychains/login.keychain"
	/usr/bin/security -v list-keychains -s "$KC"
	/usr/bin/security -v default-keychain -d user -s "$KC" || failed "Cant make login keychain $KC the default"
	/usr/bin/security -v unlock-keychain -p "$KCP" "$KC" || failed "Cant unlock keychain"
}

unlockKeychain $USER

docker build -t scornflake/couchdb-build:latest -f Dockerfile  .
docker tag scornflake/couchdb-build:latest registry.shinywhitebox.com/couchdb-build:latest
#docker push registry.shinywhitebox.com/scheduler-client:latest