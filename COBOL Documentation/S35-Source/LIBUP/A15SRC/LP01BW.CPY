      * COPYMEMBER: LIBLP/LP01BW
      **************************************************************************
      *            ( BORROWER MASTER FILE )            
      *                     LP01BW
      *  NOTE:
      *        REMEMBER TO CHANGE LPCHBW
      *
      * REV:
      *  BLV 022097 ADD BW-CREDIT-SCORE
      *  JTG 082197 ADDED BW-PRIOR-RN-FOR10 BW-LAST-RN-FOR10
      *                   BW-LAST-FORM-NO   BW-LAST-FORM-DATE
      *                   BW-OTHPHONE       BW-OTHPHONEX
      *  JTG 112097 EXPANDED BY 60 BYTES
      *  MJD 120297 EXPANDED BW-LOANS TO PIC 9(2).
      *  MJD 120597 ADDED BW-SRCD
      *  MJD 021798 ADDED SIX MISC PHONE NUMBERS, EXTENTIONS AND CODES.
      *             (66 BYTES) CODES WILL BE LINKED TO CD-FILE FOR.
      *             DESCRIPTION OF PHONE # (IE. HOME, PAGER, ETC.)
      *  BLV 031198 REMOVED BW-OTHPHONE; REPLACED WITH FILLER
      *  JTG 093098 ALLOWED BW-LIASMLN & BW-LIAOTH TO GO NEGATIVE; REGENCY REVOL
      *                 BW-LIASMLN    9(6)V99 COMP-3  TO  S9(7)V99  COMP-3
      *                 BW-LIAOTH     9(6)V99 COMP-3  TO  S9(7)V99  COMP-3
      *  KEC 100698 ADDED BW-CREDIT-LIMIT & BW-FAX-NUMBER;
      *              FOR REGENCY REVOLVING
      *  KEC 102198 REMOVED BW-FAX-NUMBER.
      *  CS  1999.0315 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 2000.0410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  BAH 010627 ADDED BW-PRIVACY-OPT-OUT FOR TFC PR# 90
      *  CS  020214 ADDED BW-CITY-TAX-CD         [WORLD PR#288]
      *  BAH 030205 ADDED BW-CALL-ON-SUNDAY FOR REGACC PR# 15
      *  KEC 030327 {PR#00026} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *             ADDED BW-ADR-PHONE-CHG-DATE, BW-BAD-PHONE-FG,
      *                   BW-BAD-ADR-FG.
      *  BAH 030724 ADDED BW-BAD-EMPPHONE-FG, REGACC PR# 36
      *  BAH 060801 ADDED BW-BAD-MISC-PHONE1-FG
      *                   BW-BAD-MISC-PHONE2-FG FOR REGACC PR# 142
      *  BAH 100608 ADDED BW-BAD-CELL-FG FOR REGACC PR# 266
      *  BAH 120830 ADDED BW-DEALTRACK-CHG-DATE FOR LOGGING CHANGES TO LNAME,
      *             FNAME,ADR1,ADR2,CITY,ST,ZIP,PHONE,EMPPHONE REGACC PR# 319
      *  BAH 120927 ADDED BW-WORKHRS-NUM REDEFINES BW-WORKHRS, MULL PR# 4092
      *  BAH 2024.1111 ADDED BW-CEASE-DESIST-DATE  BW-CEASE-DESIST-TYPE #1686
      *  BAH 2024.1114 ADDED BW-UMC-EXPIRE-DATE  BW-UMC-PR-EXPIRE-DATE #1687
      **************************************************************************
       78  BW-SIZE                     VALUE 546.
       01  BW-REC.
           03  BW1-KEY.
               05  BW-SSNO             PIC 9(9).
           03  BW2-KEY.
               05  BW-LNAME            PIC X(16).
               05  BW-FNAME            PIC X(16).
           03  BW-ADR1                 PIC X(24).
           03  BW-ADR2                 PIC X(24).
           03  BW-CITY                 PIC X(15).
           03  BW-STATE                PIC XX.
           03  BW-ZIP                  PIC X(10).
      *----------------------------------------------------
           03  BW-REFDATE              PIC 9(6)      COMP-3.
           03  BW-AGE                  PIC 9(6)      COMP-3.
           03  BW-PHONE                PIC 9(10)     COMP-3.
           03  BW-DEPEND               PIC 99        COMP-3.
           03  BW-RENTCD               PIC X(4).
           03  BW-TIMEADDR             PIC X(4).
           03  BW-CREDITCD             PIC X(4).
           03  BW-SOLICIT              PIC X.
           03  BW-STRTGCD              PIC X(6).
           03  BW-DWELLCD              PIC XX.
           03  BW-COUNTY               PIC XXX.
           03  BW-CENSUSCD             PIC X(10).
           03  BW-ORGSLSMN             PIC 9(4)      COMP-3.
           03  BW-REPCODE              PIC X(4).
      *----------------------------------------------------
           03  BW3-KEY.
               05  BW-SPSSNO           PIC 9(9).
           03  BW4-KEY.
               05  BW-SPLNAME          PIC X(16).
               05  BW-SPFNAME          PIC X(16).
      *----------------------------------------------------
           03  BW-EMPLOYER             PIC X(24).
           03  BW-EMPLPOS              PIC X(24).
           03  BW-EMPPHONE             PIC 9(10)     COMP-3.
           03  BW-EMPEXT               PIC 9(4)      COMP-3.
           03  BW-EMPYEARS             PIC 99        COMP-3.
           03  BW-EMPPAY               PIC 9(6)V99   COMP-3.
           03  BW-EMPPAYOT             PIC 9(6)V99   COMP-3.
           03  BW-EMPPAYSR             PIC X(8).
           03  BW-EMPADDR1             PIC X(24).
           03  BW-EMPADDR2             PIC X(24).
      *----------------------------------------------------
           03  BW-LOANS                PIC 9(2).
           03  BW-INTYTD               PIC S9(7)V99  COMP-3.
           03  BW-INTLYR               PIC S9(7)V99  COMP-3.
           03  BW-LIASMLN              PIC S9(7)V99  COMP-3.
           03  BW-LIAOTH               PIC S9(7)V99  COMP-3.
           03  BW-LSTPODATE            PIC 9(6)      COMP-3.
      *----------------------------------------------------
           03  BW-CONFIDENTIAL         PIC X.
           03  BW-SPAGE                PIC 9(6)      COMP-3.
           03  BW-BRANCH               PIC 9(4)      COMP-3.
           03  BW-ORIGIN               PIC X(4).
           03  BW-WORKHRS              PIC X(9).
           03  BW-WORKHRS-NUM REDEFINES BW-WORKHRS PIC 9(9).
      *----------------------------------------------------
           03  BW-PRLOANS              PIC 99.
           03  BW-RE-INTYTD            PIC S9(5)V99  COMP-3.
           03  BW-RE-INTLYR            PIC S9(5)V99  COMP-3.
           03  BW-CREDIT-SCORE         PIC 9(4)      COMP-3.
      *----------------------------------------------------
           03  BW-LAST-FORM-NO         PIC 9(3)      COMP-3.
           03  BW-LAST-FORM-DATE       PIC 9(6)      COMP-3.
      *    03  BW-OTHPHONE             PIC 9(10)     COMP-3.
      *    03  BW-OTHPHONEX           REDEFINES BW-OTHPHONE
      *                                PIC X(6).
           03  BW-CALL-ON-SUNDAY       PIC X.
           03  BW-ADR-PHONE-CHG-DATE   PIC 9(06)     COMP-6.
           03  BW-BAD-ADR-FG           PIC X(01).
           03  BW-BAD-PHONE-FG         PIC X(01).
           03  BW-PRIOR-RN-FOR10       PIC 9(6)      COMP-3.
           03  BW-LAST-RN-FOR10        PIC 9(6)      COMP-3.
           03  BW-SRCD                 PIC XX.
           03  BW-MISC-PHONES          OCCURS 6 TIMES.
               05  BW-MISC-PHONE       PIC 9(10)     COMP-3.
               05  BW-MISC-EXT         PIC 9(4)      COMP-3.
               05  BW-MISC-PHONE-CD    PIC XX.

           03  BW-CREDIT-LIMIT         PIC 9(6)V99   COMP-3.
           03  BW-PRIVACY-OPT-OUT      PIC X.
           03  BW-CITY-TAX-CD          PIC X(3).
           03  BW-BAD-EMPPHONE-FG      PIC X(01).
           03  BW-BAD-MISC-PHONE1-FG   PIC X(1).
           03  BW-BAD-MISC-PHONE2-FG   PIC X(1).
           03  BW-CELL-PHONE           PIC 9(10)     COMP-6.
           03  BW-BAD-CELL-FG          PIC X(1).
           03  BW-DEALTRACK-CHG-DATE   PIC 9(6)      COMP-6.
           03  BW-CEASE-DESIST-DATE    PIC 9(6)      COMP-6.
           03  BW-CEASE-DESIST-TYPE    PIC X.
           03  BW-UMC-EXPIRE-DATE      PIC 9(6)      COMP-6.
           03  BW-UMC-PR-EXPIRE-DATE   PIC 9(6)      COMP-6.
           03  FILLER                  PIC X(28).
