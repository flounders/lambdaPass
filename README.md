# lambdaPass

## Synopsis

A password manager written in Haskell using GPGME and JSON.

lambdaPass offers basic password management. More features will
be added over time and upon request.

## Build

To build lambdaPass do the following:

```
$ cabal install --only-dependencies
$ cabal build
```

You will then find the binary in dist/build/lambdaPass. When
the project is more stable I'll work on readying it for installation.

## Use

You will need a GPG key. I would recommend a separate one just for this
program instead of using your email key. Also you will need an empty
file to store your passwords in.

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

This will dump all of your passwords for now. Working on changing that so that
it's selective by username and location.

To remove a password:

```
$ lambdaPass -f passwords -p BDCADD4A -k "~/.gnupg" remove flounders github
```

For now the username and location are required arguments. Right now it removes
passwords if the record matches both the username and the location.