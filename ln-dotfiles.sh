#!/bin/bash

(cd emacs && ln -s "$(pwd)/init.el" ~/.config/emacs/init.el)
(cd emacs && ln -s "$(pwd)/early-init.el" ~/.config/emacs/early-init.el)
