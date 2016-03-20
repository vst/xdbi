# xdbi: Convenience Extensions to DBI Package

> **Note** that this package is experimental. Therefore, expect
> significant API changes until it reaches a certain maturity.

DBI is a powerful and the standard package to interface relational
database management systems (RBDMS). For most users, however, it might
still be cumbersome to manage database connections, send SQL queries
to and obtain their results from an RDBMS. With this package, we aim
to make some tasks easier for R users working on conventional RDBMSs.

## Quickstart

> **Note** that this package is not on CRAN yet. Therefore, we will
> install straight from GitHub.

Install `devtools` package if you don't have it installed yet:

    install.packages("devtools")

Install from github using the `develop` branch as there are currently
no releases:

    devtools::install_github("vst/xdbi", ref="develop", build_vignettes=TRUE)

Read the vignette:

    vignette("intro", package="xdbi")

## License

This library is licensed under GPLv3.

    xdbi: Convenience Extensions to DBI Package

    Copyright (C) 2015-2016 Vehbi Sinan Tunalioglu

    This program is free software: you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
