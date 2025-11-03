      * COPYMEMBER: LIBEX/EX01XBW
      **************************************************************************
      * BORROWER EXTRACTION FILE          1430 BYTES
      *
      * REGIONAL ACCEPTANCE, DATA WAREHOUSE EXTRACTION, REGACC PR# 1675
      *
      * REV:
      *  030728 BAH ADDED XBW-BAD-PHONE-FG, XBW-BAD-EMPPHONE-FG, REGACC PR# 36
      *  031114 BAH ADDED XBW-BAD-ADR-FG AND XBW-CALL-ON-SUNDAY
      *  040428 BAH ADDED ALL BX FIELDS, LENDMARK PR# 2292
      *  040903 CS  ADDED BW-REGION-NUMBER PR#76 REGIONAL
      *  060802 BAH ADDED XBW-BAD-MISC-PHONE1-FG, PHONE2-FG REGACC PR# 142
      *  080218 BAH ADDED XBW-CITY-TAX-CD , LENDMARK PR# 3155
      *  090903 BAH ADDED XBW-CELL-PHONE PL# 664
      *  BAH 100608 ADDED BW-BAD-CELL-FG FOR REGACC PR# 266
      *  BAH 2024.1111 ADDED XBW-CEASE-DESIST-DATE XBW-CEASE-DESIST-TYPE #1686
      *                AND  XBW-DEALTRACK-CHG-DATE  WAS MISSING  S35Q-145
      *  BAH 2025.0113 ADDED XBW-UMC-EXPIRE-DATE & XBW-UMC-PR-EXPIRE-DATE #1687
      *                                                          S35Q-146
      **************************************************************************
       78  XBW-SIZE                 VALUE 1430.
       01  XBW-REC.
           03  XBW-BRNO             PIC 9(04).
           03  XBW-SSNO             PIC 9(09).
           03  XBW-LNAME            PIC X(16).
           03  XBW-FNAME            PIC X(16).
           03  XBW-ADR1             PIC X(24).
           03  XBW-ADR2             PIC X(24).
           03  XBW-CITY             PIC X(15).
           03  XBW-STATE            PIC X(02).
           03  XBW-ZIP              PIC X(10).
           03  XBW-REFDATE          PIC 9(08).
           03  XBW-AGE              PIC 9(08).
           03  XBW-PHONE            PIC 9(10).
           03  XBW-DEPEND           PIC 9(02).
           03  XBW-RENTCD           PIC X(04).
           03  XBW-TIMEADDR         PIC X(04).
           03  XBW-CREDITCD         PIC X(04).
           03  XBW-SOLICIT          PIC X(01).
           03  XBW-STRTGCD          PIC X(6).
           03  XBW-DWELLCD          PIC X(02).
           03  XBW-COUNTY           PIC X(03).
           03  XBW-CENSUSCD         PIC X(10).
           03  XBW-ORGSLSMN         PIC 9(04).
           03  XBW-REPCODE          PIC X(04).
      *----------------------------------------------------
           03  XBW-SPSSNO           PIC 9(09).
           03  XBW-SPLNAME          PIC X(16).
           03  XBW-SPFNAME          PIC X(16).
      *----------------------------------------------------
           03  XBW-EMPLOYER         PIC X(24).
           03  XBW-EMPLPOS          PIC X(24).
           03  XBW-EMPPHONE         PIC 9(10).
           03  XBW-EMPEXT           PIC 9(04).
           03  XBW-EMPYEARS         PIC 9(02).
           03  XBW-EMPPAY           PIC 9(06).99.
           03  XBW-EMPPAYOT         PIC 9(06).99.
           03  XBW-EMPPAYSR         PIC X(08).
           03  XBW-EMPADDR1         PIC X(24).
           03  XBW-EMPADDR2         PIC X(24).
      *----------------------------------------------------
           03  XBW-LOANS            PIC 9(02).
           03  XBW-INTYTD           PIC +9(07).99.
           03  XBW-INTLYR           PIC +9(07).99.
           03  XBW-LIASMLN          PIC +9(07).99.
           03  XBW-LIAOTH           PIC +9(07).99.
           03  XBW-LSTPODATE        PIC 9(08).
      *----------------------------------------------------
           03  XBW-CONFIDENTIAL     PIC X(01).
           03  XBW-SPAGE            PIC 9(08).
           03  XBW-BRANCH           PIC 9(04).
           03  XBW-ORIGIN           PIC X(04).
           03  XBW-WORKHRS          PIC X(09).
      *----------------------------------------------------
           03  XBW-PRLOANS          PIC 9(02).
           03  XBW-RE-INTYTD        PIC +9(05).99.
           03  XBW-RE-INTLYR        PIC +9(05).99.
           03  XBW-CREDIT-SCORE     PIC 9(04).
      *----------------------------------------------------
           03  XBW-LAST-FORM-NO     PIC 9(03).
           03  XBW-LAST-FORM-DATE   PIC 9(08).
           03  XBW-PRIOR-RN-FOR10   PIC 9(08).
           03  XBW-LAST-RN-FOR10    PIC 9(08).
           03  XBW-SRCD             PIC X(02).
           03  XBW-MISC-PHONE-TAB.
               05  XBW-MISC-PHONE1      PIC 9(10).
               05  XBW-MISC-EXT1        PIC 9(04).
               05  XBW-MISC-PHONE-CD1   PIC X(02).
               05  XBW-MISC-PHONE2      PIC 9(10).
               05  XBW-MISC-EXT2        PIC 9(04).
               05  XBW-MISC-PHONE-CD2   PIC X(02).
               05  XBW-MISC-PHONE3      PIC 9(10).
               05  XBW-MISC-EXT3        PIC 9(04).
               05  XBW-MISC-PHONE-CD3   PIC X(02).
               05  XBW-MISC-PHONE4      PIC 9(10).
               05  XBW-MISC-EXT4        PIC 9(04).
               05  XBW-MISC-PHONE-CD4   PIC X(02).
               05  XBW-MISC-PHONE5      PIC 9(10).
               05  XBW-MISC-EXT5        PIC 9(04).
               05  XBW-MISC-PHONE-CD5   PIC X(02).
               05  XBW-MISC-PHONE6      PIC 9(10).
               05  XBW-MISC-EXT6        PIC 9(04).
               05  XBW-MISC-PHONE-CD6   PIC X(02).
           03  XBW-MISC-PHONES REDEFINES XBW-MISC-PHONE-TAB
                                        OCCURS 6 TIMES.
               05  XBW-MISC-PHONE   PIC 9(10).
               05  XBW-MISC-EXT     PIC 9(04).
               05  XBW-MISC-PHONE-CD PIC X(02).
           03  XBW-CREDIT-LIMIT     PIC 9(06).99.
           03  XBW-PRIVACY-OPT-OUT  PIC X(01).
           03  XBW-CONTACT-PHONE-TAB.
               05  XBW-CONTACT-DESC1    PIC X(46).
               05  XBW-CONTACT-PHONE1   PIC 9(10).
               05  XBW-CONTACT-EXT1     PIC 9(04).
               05  XBW-CONTACT-DESC2    PIC X(46).
               05  XBW-CONTACT-PHONE2   PIC 9(10).
               05  XBW-CONTACT-EXT2     PIC 9(04).
               05  XBW-CONTACT-DESC3    PIC X(46).
               05  XBW-CONTACT-PHONE3   PIC 9(10).
               05  XBW-CONTACT-EXT3     PIC 9(04).
               05  XBW-CONTACT-DESC4    PIC X(46).
               05  XBW-CONTACT-PHONE4   PIC 9(10).
               05  XBW-CONTACT-EXT4     PIC 9(04).
               05  XBW-CONTACT-DESC5    PIC X(46).
               05  XBW-CONTACT-PHONE5   PIC 9(10).
               05  XBW-CONTACT-EXT5     PIC 9(04).
               05  XBW-CONTACT-DESC6    PIC X(46).
               05  XBW-CONTACT-PHONE6   PIC 9(10).
               05  XBW-CONTACT-EXT6     PIC 9(04).
               05  XBW-CONTACT-DESC7    PIC X(46).
               05  XBW-CONTACT-PHONE7   PIC 9(10).
               05  XBW-CONTACT-EXT7     PIC 9(04).
               05  XBW-CONTACT-DESC8    PIC X(46).
               05  XBW-CONTACT-PHONE8   PIC 9(10).
               05  XBW-CONTACT-EXT8     PIC 9(04).
               05  XBW-CONTACT-DESC9    PIC X(46).
               05  XBW-CONTACT-PHONE9   PIC 9(10).
               05  XBW-CONTACT-EXT9     PIC 9(04).
               05  XBW-CONTACT-DESC10   PIC X(46).
               05  XBW-CONTACT-PHONE10  PIC 9(10).
               05  XBW-CONTACT-EXT10    PIC 9(04).
           03  XBW-CONTACT-PHONES REDEFINES XBW-CONTACT-PHONE-TAB
                                        OCCURS 10 TIMES.
               05  XBW-CONTACT-DESC     PIC X(46).
               05  XBW-CONTACT-PHONE    PIC 9(10).
               05  XBW-CONTACT-EXT      PIC 9(04).
           03  XBW-BAD-PHONE-FG         PIC X.
           03  XBW-BAD-EMPPHONE-FG      PIC X.
           03  XBW-BAD-ADR-FG           PIC X.
           03  XBW-CALL-ON-SUNDAY       PIC X.

           03  XBX-EMAIL                PIC X(40).

           03  XBX-SCORE-TABLE.
               05  XBX-CREDIT-SCORE     PIC 9(4) OCCURS 4.
               05  XBX-BNKRPT-SCORE     PIC 9(4) OCCURS 4.

           03  XBX-ALT-ADDRESS.
               05  XBX-ALT-ADR1             PIC X(24).
               05  XBX-ALT-ADR2             PIC X(24).
               05  XBX-ALT-CITY             PIC X(15).
               05  XBX-ALT-STATE            PIC X(2).
               05  XBX-ALT-ZIP              PIC X(10).

           03  XBW-REGION-NUMBER        PIC 9(2).

           03  XBW-BAD-MISC-PHONE1-FG   PIC X.
           03  XBW-BAD-MISC-PHONE2-FG   PIC X.

           03  XBW-CITY-TAX-CD          PIC X(3).
           03  XBW-CELL-PHONE           PIC 9(10).
           03  XBW-BAD-CELL-FG          PIC X.

           03  XBW-DEALTRACK-CHG-DATE   PIC 9(8).
           03  XBW-CEASE-DESIST-DATE    PIC 9(8).
           03  XBW-CEASE-DESIST-TYPE    PIC X.
           03  XBW-UMC-EXPIRE-DATE      PIC 9(8).
           03  XBW-UMC-PR-EXPIRE-DATE   PIC 9(8).
           03  XBW-UNUSED               PIC X(18).

           03  XBW-CR                   PIC X(01).
           03  XBW-LF                   PIC X(01).
