#!/bin/sh

scribble --htmls one-more-re-nightmare.scrbl
for p in one-more-re-nightmare/*.html; do
    sed -i -e 's/<!DOCTYPE[^>]*>/<!DOCTYPE html>/' \
           -e 's/initial-scale=0.8/initial-scale=1.0/' \
           -e "s/{}&rsquo;/{}'/g" \
        "$p"
done
cat spec-macros.css mobile-view.css >> one-more-re-nightmare/scribble-style.css
