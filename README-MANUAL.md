# The Hard Way
aka: load up an ubuntu16.04 VM, get couchdb on it, this code, and compile away.

It works, but I only found out at deployment time that actually the 2.1.2 couchdb doesn't use OTP 19, it uses OTP 17.
erlang:system_time exists as of 18, so changed that as well.  Makes me wonder how this ever worked?

Anyway, if you want to build manually:

- Requires:
    - couchdb be installed, since it'll reference the couch_db.hrl include (and will need couch libs during tests)
    - ububtu16.04, because you want openssl1.0.0 not the 1.1 of later distros (erlang 19.3 expects 1.0.0)

- *sudo apt-get install rebar* (just rebar, not rebar3, despite the warnings that rebar is deprecated)
- export ERL_LIBS=/opt/couchdb/lib/couch-2.1.2-RC8
    - so that it picks up the couch code when running the tests (and doesn't fail with undefs)
    - so that it picks up the couch includes when compiling
    - ERL_LIBS also affects the BEAM codepaths, so this is how you can tell it where compiled code is.

- Install couchdb 2.1.2 (latest at the time of writing)
    - echo "deb https://apache.bintray.com/couchdb-deb xenial main" | sudo tee -a /etc/apt/sources.list
    - curl -L https://couchdb.apache.org/repo/bintray-pubkey.asc | sudo apt-key add -
    - sudo apt-get update
    - auto apt-get install couchdb

- Get erlang 19.3 (ubuntu 16.04):
    - wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
    - sudo dpkg -i erlang-solutions_1.0_all.deb
    - sudo apt-get update
    - sudo apt-get install esl-erlang=1:19.3
    - Check it: "erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell"

Now you should be able to build and run the tests of the auth module.
    - rebar co (because one test fails, and I dont know how to fix the matcher)
    - ./install.sh
    - sudo systemctl restart couchdb

