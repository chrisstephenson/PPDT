#!/bin/bash
cat shakescw.txt |tr -cs A-Za-z '
'|tr A-Z a-z|sort|uniq -c|sort -rn|sed 500q
