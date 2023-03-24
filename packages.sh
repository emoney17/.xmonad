#!/bin/bash

pkgs=$(pacman -Q | wc -l)
echo "Pkgs: $pkgs" 
