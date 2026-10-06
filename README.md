# json2dir-roc

json2dir in Roc, on the basic-cli platform; `["link", ...]` and `["script", ...]` documents are rejected because basic-cli cannot create symlinks or set execute permissions.

    roc build --opt=speed --output=out/json2dir main.roc && out/json2dir < document.json
