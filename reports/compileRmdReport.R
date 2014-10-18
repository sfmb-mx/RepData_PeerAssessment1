### compileRmdReport.R --- 
## 
## Filename: compileRmdReport.R
## Description: 
## Author: Sergio-Feliciano Mendoza-Barrera
## Maintainer: 
## Created: Fri Oct 17 21:31:58 2014 (-0500)
## Version: 
## Package-Requires: ()
## Last-Updated: Fri Oct 17 22:12:58 2014 (-0500)
##           By: Sergio-Feliciano Mendoza-Barrera
##     Update #: 5
## URL: 
## Doc URL: 
## Keywords: 
## Compatibility: 
## 
######################################################################
## 
### Commentary: 
## 
## 
## 
######################################################################
## 
### Change Log:
## 
## 
######################################################################
## 
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or (at
## your option) any later version.
## 
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
## 
######################################################################
## 
### Code:

library(knitr)
knit2html("./PA1_template.Rmd")
browseURL("./PA1_template.html")

######################################################################
### compileRmdReport.R ends here
