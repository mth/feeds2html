.. ex: se sw=4 sts=4 expandtab:

============
 Feeds2HTML
============

----------------------------------
 RSS/Atom feeds to HTML converter
----------------------------------

:Author: Madis Janson
:Version: Feeds2HTML 1.0
:Manual section: 1
:Manual group: Feeds2HTML manual

SYNOPSIS
========

feeds2html configuration

DESCRIPTION
===========

Feeds2HTML is an utility to fetch and combine multiple RSS (or Atom)
feeds specified in the configuration. The items from all feeds are sorted
by their date, formatted according to the given configuration and written
into standard output.

It is mostly intended to be used with web server by treating the
configuration files as CGI scripts (where the feeds2html utility is
used as an interpretator). The utility caches both the raw RSS/Atom
XML files and the generated result in ``/tmp/.rss-cache-``\ **UID** file
to aid this usage.

CONFIGURATION
=============

blaah
