# Makefile for XEmacs base lisp code

# This file is part of XEmacs.

# XEmacs is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.

# XEmacs is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.

# You should have received a copy of the GNU General Public License
# along with XEmacs; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

VERSION = 1.34
AUTHOR_VERSION =
MAINTAINER = XEmacs Development Team <xemacs-beta@xemacs.org>
PACKAGE = xemacs-base
PKG_TYPE = regular
REQUIRES =
CATEGORY = libs

ELCS = add-log.elc advice.elc annotations.elc assoc.elc case-table.elc \
	chistory.elc comint.elc comint-xemacs.elc compile.elc debug.elc \
	ebuff-menu.elc echistory.elc ehelp.elc edmacro.elc electric.elc \
	enriched.elc env.elc facemenu.elc ffap.elc helper.elc imenu.elc \
	iso-syntax.elc macros.elc novice.elc outline.elc \
	passwd.elc pp.elc regi.elc ring.elc shell.elc skeleton.elc sort.elc \
	thing.elc time-stamp.elc timezone.elc xbm-button.elc xpm-button.elc

DATA_FILES = etc/enriched.doc
DATA_DEST = .

include ../../XEmacs.rules

GENERATED += custom-load.elc

all:: $(ELCS) auto-autoloads.elc custom-load.elc

srckit: srckit-std

binkit: binkit-common
