# eTrexChop
Command-line Utility to Trim Track Points out of an eTrex 30x GPX File

        1        2        3        4        5        6        7
12345789012345789012345789012345789012345789012345789012345789012
Only supports trimming from the front of the GPX file with zero
offset right now.  I will fix the rest of the issues eventually,
but I will personally be using this tool when I don't have
access to Garming Basecamp and I just want to trim some points 
from the front and (eventually back) of an eTrex GPX file.

Problem is I sometimes have to start my eTrex before I start
my event, so I have non-relvent data at the first of the
records.  Also, there is often a delay from when I end my event
to when I can turn the eTrex off so there is also non-relevent
data at the end of the file.

This simple "hack" (when finished) will allow me to do that 
without using Garmin Basecamp.  Part of the reason I wrote this
"hack" is because, there is no Garmin Basecamp for GNU\Linux that
I'm aware of.

Yes, I can edit the GPX files via EMACS, but this is easier for
me.
