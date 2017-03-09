#!/bin/bash
curl -s http://wiki.audacityteam.org/wiki/Nyquist_Effect_Plug-ins | grep -o 'http://[^"]*' | grep -F ".ny" >cur && wget -i  cur

curl -s http://wiki.audacityteam.org/wiki/Nyquist_Generate_Plug-ins  | grep -o 'http://[^"]*' | grep -F ".ny" >gen && wget -i  gen

curl -s http://wiki.audacityteam.org/wiki/Nyquist_Analyze_Plug-ins | grep -o 'http://[^"]*' | grep -F ".ny" >anal && wget -i  anal

