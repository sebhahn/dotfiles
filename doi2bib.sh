#!/bin/bash
curl -LH "Accept: application/x-bibtex;q=1" http://dx.doi.org/$1
echo
