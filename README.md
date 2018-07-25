# Getting it going on CouchDB 2.1.2

I know next to zero erlang. Perhaps all of below is obvious to erlangers.

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

- Get erlang 19.3:
    - wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
    - sudo dpkg -i erlang-solutions_1.0_all.deb
    - sudo apt-get update
    - sudo apt-get install esl-erlang=1:19.3
    - Check it: "erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell"

Now you should be able to build and run the tests of the auth module.
    - rebar co (because one test fails, and I dont know how to fix the matcher)
    - ./install.sh
    - sudo systemctl restart couchdb

# Things I modified
    - Added dependencies on jiffy / jsx. JSX pinned to 2.8.0 as that is what ejwt requires.
    - Added script to deploy ebin directly to /opt/couch/lib/couch_jwt_auth (mainly to test the thing)


# couch_jwt_auth

couch_jwt_auth is authentication plugin for CouchDB. It accepts JSON Web Token in the Authorization
HTTP header and creates CouchDB user context from the token information. couch_jwt_auth doesn't use
CouchDB authentication database. User roles are read directly from JWT and not from the
authentication database.

The plugin doesn't support unsecured JWTs or digital signature algorithms. Only hash-based message
authentication codes are supported. I might add support for digital signature algorithms later. 

If you want to learn more around JWT itself, [the intro](https://jwt.io/introduction/) on their
site is just amazing and explains the concepts really well.

## Installation

1. Install `rebar` if you don't already have it, which is used to compile the Erlang project.
  ```
  $ brew install rebar
  ```

2. Clone (download) the repo:
  ```
  $ git clone https://github.com/UXtemple/couch_jwt_auth.git
  ```

3. Build the plugin:
  ```
  $ cd couch_jwt_auth
  $ ./build.sh
  ```

  Your plugin will be ready in the folder `dist`.

4. Find where CouchDB is installed:
  ```
  $ brew info couchdb | grep Cellar
  ```

  It should ouput something like: `/usr/local/Cellar/couchdb/1.6.1_3 (657 files, 17M) *`, a path like
  the one at the beginning (`/usr/local/Cellar/couchdb/1.6.1_3`) is what you're after :). That's
  CouchDB's root in your Mac. Use whatever your local path is in the following commands:

5. Ensure the `plugins` directory exists:
  ```
  $ mkdir -p /usr/local/Cellar/couchdb/1.6.1_3/lib/couchdb/plugins
  ```

6. Move the plugin to CouchDB's plugins folder:
  ```
  $ mv dist /usr/local/Cellar/couchdb/1.6.1_3/lib/couchdb/plugins/couch_jwt_auth
  ```

7. Configure the couch_jwt_auth:

  Find CouchDB's config. It may live at `/usr/local/etc/couchdb/local.ini` or
  `/etc/couchdb/local.ini`.

  Edit it and add `couch_jwt_auth` to CouchDB's `authentication_handlers` `httpd` section.
  It's ok to have more options on that line :):

  ```
  [httpd]
  authentication_handlers = ..., {couch_jwt_auth, jwt_authentication_handler}, ...
  ```

  After that, add a section to the same config file that looks like this:

  ```
  [jwt_auth]
  secret = supersecret
  name_claim = name
  roles_claim = roles
  ```

  [Here's a nice command to get a random
  secret](http://security.stackexchange.com/questions/81976/is-this-a-secure-way-to-generate-passwords-at-the-command-line):

  ```
  openssl rand -base64 32 | tr -d /=+ | cut -c -30
  ```

8. Restart couchdb and you're good to go.

## Test with Curl

```
$ curl http://127.0.0.1:5984/_session
```

You should see `jwt` in the authentication_handlers. Next
you can test sending JWT with the request. The secret for this test is `secret`.

Now you can generate a sample JWT from http://jwt.io. The token is included in the Authorization
HTTP header like this:

```
$ curl -H "Authorization: Bearer TOKEN_HERE" http://127.0.0.1:5984/_session
```

With default options you should see the JWT "sub" claim content in the CouchDB username:

```
$ curl -v -H 'Authorization: Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJuYW1lIjoiU29tZSBVc2VyIiwicm9sZXMiOltdfQ.v4QRSYnAOen_NMBzlMER_Jrkep0xEz2kL09KscALC_c' http://localhost:5984/_session
```

will output:

```
$ {"ok":true,"userCtx":{"name":"Some User","roles":[]},"info":{"authentication_db":"_users","authentication_handlers":["oauth","cookie","default","jwt"],"authenticated":"jwt"}}
```

## How do I create JWT tokens to use with this?

You can use this together with [jwt-couchdb](https://github.com/UXtemple/jwt-couchdb) to
leverage authentication through the `_users` db. It's a very simple endpoint that will create a JWT
token from a valid CouchDB user.

Alternatively, you can use a service like [Auth0](https://auth0.com/) which is an identity service
that supports many identity providers like Google, Facebook, Twitter and so on.
Auth0 generates a JWT that can be parsed by this plugin.
[Here](https://github.com/softapalvelin/getting-started-todo) is a sample application that uses
Auth0 to authenticate a user with CouchDB.

[The motivation section in
couchdb-jtw-auth-server](https://github.com/BeneathTheInk/couchdb-jwt-auth-server#motivation)
explains very well how and why you would want to use this approach over cookies.

## Blacklisting tokens

If you want to blacklist tokens, just add a section to CouchDB's config named `jwt_auth_blacklist`
and add the tokens together with a reason (for your records if you want) like this:

```ini
[jwt_auth_blacklist]
eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJuYW1lIjoiU29tZSBVc2VyIiwicm9sZXMiOltdfQ.v4QRSYnAOen_NMBzlMER_Jrkep0xEz2kL09KscALC_c = bad guy one
```

Forked from https://github.com/softapalvelin/couch_jwt_auth by UXtemple.
Apache v2.0 license.
