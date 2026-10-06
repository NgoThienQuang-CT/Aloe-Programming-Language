#!/usr/bin/bash

menhir lib/parser.mly --list-errors > /tmp/new.messages
menhir lib/parser.mly --merge-errors lib/parser.messages --merge-errors /tmp/new.messages > lib/parser.messages.merged
mv lib/parser.messages.merged lib/parser.messages
rm /tmp/new.messages