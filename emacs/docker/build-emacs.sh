#!/usr/bin/env bash

set -x
set -e

DOCKER_FILE="centos-7-Dockerfile"
IMAGE_NAME="jgarvin/centos-7-emacs"
#DOCKER_FILE="ubuntu-Dockerfile"
#IMAGE_NAME="jgarvin/ubuntu-emacs"
SCRIPT_INSTALL_DIRECTORY="$HOME/opt2/bin"
EMACS_INSTALL_DIRECTORY="$HOME/emacs-install"

mkdir -p ${SCRIPT_INSTALL_DIRECTORY}

rm -rf "$HOME/emacs-install"
# if we don't create this ourself then the docker daemon will and it will be owned by root!
mkdir -p ${EMACS_INSTALL_DIRECTORY}

# build the image
docker build -f $DOCKER_FILE -t ${IMAGE_NAME} .

# copy the install out of the image, and run the container as the
# current user to make sure we will be able to do the copying
docker run --rm -u $(id -u):$(id -g) -v  $EMACS_INSTALL_DIRECTORY:/emacs-install-out ${IMAGE_NAME} bash -c "cp -r /emacs-install/* /emacs-install-out; mkdir /emacs-install-out/system-lib; cp -r /lib64/*.so* /emacs-install-out/system-lib"

# create a script for running with appropriate paths set...
# leading backslash prevents variable expansion
cat << \EOF > ${SCRIPT_INSTALL_DIRECTORY}/emacs
#!/usr/bin/env bash
export PATH=$HOME/emacs-install/bin:$PATH
export LD_LIBRARY_PATH=$HOME/emacs-install/lib:$HOME/emacs-install/system-lib:$LD_LIBRARY_PATH
emacs "$@"
EOF
chmod +x ${SCRIPT_INSTALL_DIRECTORY}/emacs

cat << \EOF > ${SCRIPT_INSTALL_DIRECTORY}/emacsclient
#!/usr/bin/env bash
export PATH=$HOME/emacs-install/bin:$PATH
export LD_LIBRARY_PATH=$HOME/emacs-install/lib:$HOME/emacs-install/system-lib:$LD_LIBRARY_PATH
emacsclient "$@"
EOF
chmod +x ${SCRIPT_INSTALL_DIRECTORY}/emacsclient
