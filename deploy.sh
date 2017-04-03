#!/bin/sh

./site build
rm -rf _site/* && cp -r tmp/* _site
