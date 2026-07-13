# Friendly Technical Testing Guide

## Purpose

This is an early technical test of the Sabah Climate Risk Explorer prototype.

The purpose is to find out whether the app workflow is understandable, whether the results make sense, and where the app is confusing or fragile.

This is not a public launch and not a final decision-making tool.

## Testing setup

The app will be run locally from Colin's or Marcolm's computer.

Testers do not need to install R, Rstudio, GitHub files or raster data during the session.

## What we want feedback on

- AOI selection.
- result table clarity.
- comparison table clarity.
- graph usefulness.
- cropped raster option.
- downloads.
- Data Availability tab.
- WBGT interpretation.
- confusing labels or instructions.
- bugs or crashes.

## What is out of scope

- public deployment.
- installer setup
- final risk scoring.
- full scientific validation.
- adding all possible variables.

## Main demo workflow

AOI: Jambongan
Variable: Fire probability
Comparison: On
Scenarios: Baseline and SSP2-4.5
Periods: 1981-2010 and 2041-2070
Cropped raster output: OFF

## Optional demo workflow

AOI: Jambongan
Variable: Maximum WBGT
Comparison: OFF first
Cropped raster output: On

WBGT should be explained as monthly average maximum WBGT, not daily extreme WBGT.

## Backup workflow

If upload, draw polygon, point-buffer, WBGT or cropped raster fails, use:

Jambongan → Fire Probability → comparison → graph → comparison CSV.
