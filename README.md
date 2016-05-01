# lambdaPass

## Synopsis

A password manager written in Haskell using GPGME, JSON and YAML.

lambdaPass attempts to keep your metadata hidden from prying eyes
as well as passwords, so all your account information is stored in
one file that is encrypted.

## Dependencies

You will need the C headers for GPGME, and the standard C++ libary
for some Haskell packages. On Ubuntu you can install GPGME's headers
by running `sudo apt-get install libgpgme11-dev`, but other package
managers will obviously be different. Most distributions should have
the standard C++ library installed, but you mileage may vary. Haskell
packages will be supplied by stack, so you will need that installed
as. To install Haskell stack: 
[http://docs.haskellstack.org/en/stable/install_and_upgrade/](http://docs.haskellstack.org/en/stable/install_and_upgrade/)

## Build

To build lambdaPass do the following:

```
$ stack setup
$ stack install
```

You will then find the binary in ~/.local/bin. This location can be
changed using stack's options.

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

### Adding a Password

To add a password:

```
$ lpass -f passwords -p BDCADD4A -k "~/.gunpg" add -u flounders -l github
```

This will then prompt for the password from IO. It would be very bad to
have your passwords in your command history. If you type in `lpass add -h`, the
help will explain the options. -f, -p and -k are always needed, however they 
can be supplied from a config file that lpass creates upon first run. You can
find it in ~/.config/lambdaPass/lambdaPass.yml. The following examples will
assume you have this config file setup. Setup of the config will be covered
furthur down.

### Viewing Passwords

To view your passwords:

```
$ lpass view -u flounders -l github
```

This will show the password for accounts that contain both flounders in the
username and github in the location. If you only have one github account
you may just want to specify location alone. If you want to show notes or
usernames you may have forget, see the fields option for `lpass view -h`.

### Updating Account Info

To update account info you will do something like the following:

```
$ lpass update -u flounders -l github --uP
```

This will then prompt for a new password to be entered. Again we don't want to
leave sensitive things like passwords in our command history. You can use -u,
-l, and -n to select accounts you wish to update. The updated info will be
applied to all accounts that match your selection. To see information on
specific options run `lpass update -h`.

### Removing Passwords

To remove an account:

```
$ lpass remove -u flounders -l github
```

Accounts that match the username "flounders" and the location "github" will be
removed. You do not have to use username and location together to delete accounts
but it is recommended. If you don't supply either a username or a location
nothing will be removed. If you were to only supply -u flounders and you have
multiple accounts with that username it will remove all accounts that match.

### lambdaPass.yml

You can omit the -f -p and -k required options if you have setup lambdaPass.yml.
The file should be created if you have run lpass at all. You should see the
following:

```
base:
```

To avoid using the -f, -p and -k flags add the following:

```
base:
  passFile: /home/user/.local/lambdaPass/passwords
  gpgDir: /home/user/.gnupg
  gpgKey: 12345678
```

passFile maps to -f, gpgKey to -p, and gpgDir to -k. Should you for some reason
only want to supply the fingerprint of the key you are using in the config and
always use the file and directory options at the command prompt you can do that.
Just keep in mind that lpass needs to have this information to work, otherwise,
it will tell you what's missing and do nothing. The order in the config file
doesn't matter, but it does need to be under base. For formatting questions
consult the YAML standard.

Absolute paths are really important at this time. Relative paths will be supported
eventually.

## Input

Your input on the program is greatly desired and appreciated. You can either email
me at steven.m.williams at yandex dot com or you can open an issue.
