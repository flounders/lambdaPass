# lambdaPass

## Synopsis

A password manager written in Haskell using GPGME and JSON.

lambdaPass offers basic password management. More features will
be added over time and upon request.

## Dependencies

You will need the C headers for GPGME, and the standard C++ libary
for some Haskell packages. On Ubuntu you can install GPGME's headers
by running `sudo apt-get install libgpgme11-dev`, but other package
managers will obviously be different. Most distributions should have
the standard C++ library installed, but you mileage may vary. Other
dependencies are aeson, h-gpgme and optparse-applicative which
Cabal will install for you using the commands below.

## Build

To build lambdaPass do the following:

```
$ cabal update
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal build
```

You will then find the binary in dist/build/lambdaPass. When
the project is more stable I'll work on readying it for installation.

## Use

You will need a GPG key. I would recommend a separate one just for this
program instead of using your email key. Also you will need an empty
file to store your passwords in.

To create a key:

```
$ gpg --gen-key
```

or if you are looking for more options

```
$ gpg --full-gen-key
```

To add a password:

```
$ lambdaPass -f passwords -p BDCADD4A -k "~/.gunpg" add flounders github none
```

This will then prompt for the password from IO. It would be very bad to
have your passwords in your command history. Right now it echoes back what
you type, so I wouldn't use the program with someone over your shoulder right
now. If you type in just `lambdaPass add`, the help will explain the options,
but just to explain this for now -p is for the fingerprint of the key you want
to use, and -k is the location of that key.

To view your passwords:

```
$ lambdaPass -f passwords -p BDCADD4A -k "~/.gnupg" view
```

This will dump all of your passwords by default. You can filter by username,
location or username and location together. Run `lambdaPass view` to see format
specifics.

To remove a password:

```
$ lambdaPass -f passwords -p BDCADD4A -k "~/.gnupg" remove flounders github
```

For now the username and location are required arguments. Right now it removes
passwords if the record matches both the username and the location.
