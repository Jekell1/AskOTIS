      * COPYMEMBER: LIBCL/SUMMBYW
      **********************************************************
      *          LIBCL/SUMMBYW
      *
      * USED TO DYNAMICALLY BUILD A WORK BRANCH STAT RECORD:
      *
      * REV:
      *  MJD 021296 ADDED SUMMBY-TYPE. 1 - RETURNS REC WITH O2
      *                                2 - RETURNS REC LESS O2
      *                                3 - RETURNS O2 AMT ONLY
      *                                4 - RETURNS INT AMT ONLY
      *  BLF 041004 ADDED BEG & ENDING CLASS
      *  BAH 2015/12/08 EXPANDED DATE TO 9(8) AND CLASS TO 9(3), PD0003
      **********************************************************
       01  SUMMBY-LINK.
           03  SUMMBY-DATE              PIC 9(8).
           03  SUMMBY-BRNO              PIC X(4).
           03  SUMMBY-TYPE              PIC 9.
           03  SUMMBY-CLASS-RANGE.
               05  SUMMBY-BEG-CLASS     PIC 9(3).
               05  SUMMBY-END-CLASS     PIC 9(3).
