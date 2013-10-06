#!/bin/bash

scp -r lang/ ssh.cs.brown.edu:~/web/public/
ssh ssh.cs.brown.edu chmod -R a+r ~/web/public/lang/*
