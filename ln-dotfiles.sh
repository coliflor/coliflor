#!/bin/bash

(cd emacs && ln -s "$(pwd)/init.el" ~/.emacs.d/init.el)
(cd emacs && ln -s "$(pwd)/early-init.el" ~/.emacs.d/early-init.el)
