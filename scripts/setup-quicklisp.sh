#!/bin/bash -xe


tmpdir=quicklisp-install-dir-$$

rm -rf $tmpdir
mkdir -p $tmpdir

pushd $tmpdir

if [[ ! `which sbcl` ]]; then
    brew install sbcl
fi

if [[ ! `which gpg` ]]; then
    brew install gpg
fi

# Make sure we have the public key of quicklisp
curl -O https://beta.quicklisp.org/release-key.txt
gpg --import release-key.txt

#download and verify the init file
curl -O https://beta.quicklisp.org/quicklisp.lisp
curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
gpg --verify quicklisp.lisp.asc quicklisp.lisp

echo 'Enter the following commands'
echo '(quicklisp-quickstart:install)'
echo '(ql:add-to-init-file)'
echo '(ql:quickload "quicklisp-slime-helper")'
echo '(quit)'

sbcl --load quicklisp.lisp

popd
rm -rf $tmpdir
