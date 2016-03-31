!    FORTRAN interface to the C library udunits2 
!        the C library udunits2 (C) is Copyright UCAR/Unidata
!        this fortran Interface is (C) is Copyright Université du Québec à Montréal
!     Redistribution and use in source and binary forms, with or without modification,
!     are permitted provided that the following conditions are met:
! 
!       1) Redistributions of source code must retain the above copyright notice,
!           this list of conditions and the following disclaimer.
!       2) Redistributions in binary form must reproduce the above copyright notice,
!           this list of conditions and the following disclaimer in the documentation
!           and/or other materials provided with the distribution.
!       3) Neither the names of the development group, the copyright holders, nor the
!           names of contributors may be used to endorse or promote products derived
!           from this software without specific prior written permission.
!       4) This license shall terminate automatically and you may no longer exercise
!           any of the rights granted to you by this license as of the date you
!           commence an action, including a cross-claim or counterclaim, against
!           the copyright holders or any contributor alleging that this software
!           infringes a patent. This termination provision shall not apply for an
!           action alleging patent infringement by combinations of this software with
!           other software or hardware.
! 
!     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!     IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!     FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE CONTRIBUTORS
!     OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
!     WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!     CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE SOFTWARE.
! documentation for c code : 
! http://www.unidata.ucar.edu/software/udunits/udunits-2.0.4/udunits2lib.html
	type, bind(C) :: UT_SYSTEM_PTR
	type(C_PTR) :: ptr
	end type

	type, bind(C) :: UT_UNIT_PTR
	type(C_PTR) :: ptr
	end type

	type, bind(C) :: CV_CONVERTER_PTR
	type(C_PTR) :: ptr
	end type

	type, bind(C) :: UT_STATUS
	integer(C_INT) :: value
	end type
	
	type :: CHAR_STAR
	character(len=1),dimension(:),pointer :: ptr
	end type
!
!   the following constants are lifted from C enum in udunits2.h
!   and may have to be adjusted should any change be made to that file
!
!   a cleaner way would be to use a C program to produce this file
!
	integer, parameter :: UT_ASCII = 0
	integer, parameter :: UT_ISO_8859_1 = 1
	integer, parameter :: UT_LATIN1 = UT_ISO_8859_1
	integer, parameter :: UT_UTF8 = 2

	integer, parameter :: UT_NAMES = 4
	integer, parameter :: UT_DEFINITION = 8

	integer, parameter :: UT_SUCCESS  =0
	integer, parameter :: UT_BAD_ARG  =1
	integer, parameter :: UT_EXISTS  =2
	integer, parameter :: UT_NO_UNIT  =3
	integer, parameter :: UT_OS  =4
	integer, parameter :: UT_NOT_SAME_SYSTEM  =5
	integer, parameter :: UT_MEANINGLESS  =6
	integer, parameter :: UT_NO_SECOND  =7
	integer, parameter :: UT_VISIT_ERROR  =8
	integer, parameter :: UT_CANT_FORMAT  =9
	integer, parameter :: UT_SYNTAX  =10
	integer, parameter :: UT_UNKNOWN  =11
	integer, parameter :: UT_OPEN_ARG  =12
	integer, parameter :: UT_OPEN_ENV  =13
	integer, parameter :: UT_OPEN_DEFAULT  =14
	integer, parameter :: UT_PARSE  =15
