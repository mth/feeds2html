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

feeds2html configuration-file

DESCRIPTION
===========

Feeds2HTML is an utility to fetch and combine multiple RSS (or Atom)
feeds specified in the configuration. The items from all feeds are sorted
by their date, formatted according to the given configuration and written
into standard output.

It is mostly intended to be used with web server by treating the
configuration files as CGI scripts (where the feeds2html utility is
used as an interpretator). The utility caches both the raw RSS/Atom
XML files and the generated result in ``/tmp/.rss-cache-``\ **$UID**
directory to aid this usage.

The idea is that for most of the RSS, you'll need a web browser anyway to read
the linked articles, so the minimal user interface for RSS reader can be
a simple web page. This utility provides a simple way to have such RSS reader,
together with ability to merge multiple RSS feeds into single list, where newest
items are on the top. It can work with any browser, including the ones running
in terminal (like lynx, which eliminates most noise from the modern web).

CONFIGURATION
=============

blaah
