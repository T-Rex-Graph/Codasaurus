#Dr. Richard Watson, Dr. Xia Zhao, Yuanyuan Song, Nathaniel Kelley
#Author: Nathaniel Kelley
#Nathaniel Kelley is a research assistant at the University of Georgia, Terry College of Business (2020-2021) and the author of the first version of Codasaurus.
#Author email: nkelley1998@gmail.com
#Sponsor email: rwatson@uga.edu
#T-Rex Research team at the University of Georgia, Terry College of Business

#Copyright 2021 University of Georgia, Terry College of Business

#This file is part of Codasaurus.

#Codasaurus is free software: you can redistribute it and/or modify
#it under the terms of the GNU Affero General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.

#Codasaurus is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU Affero General Public License for more details.

#You should have received a copy of the GNU Affero General Public License
#along with Codasaurus.  If not, see <https://www.gnu.org/licenses/>.

############################################################################

# In this script include packages, functions, datasets and anyting that will be
# used both by UI and server

############################.
##Packages ----
############################.
library(shiny)
library(rorcid)
library(rcrossref)
library(stringr)
library(readr)
library(uuid)
