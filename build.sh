rebar get-deps co eu
#rebar get-deps co
rm -rf dist
mkdir -p dist/priv
mv ebin dist
# fix some incompatibility with couchdb plugins and rebar deps
for file in deps/*/ebin/*; do cp "$file" "dist/ebin/$(basename $file)";done
for file in deps/*/priv/*.so; do cp "$file" "dist/priv/$(basename $file)";done
