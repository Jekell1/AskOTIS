      * COPYMEMBER: LIBLP/LP01FD
      ******************************************************************
      *     FORM DESCRIPTOR & DATA BASE FILE       BYTES = 2828     
      *
      *     KEY 1 - FMNO                                
      *     KEY 2 - DCODE, DBNO                         
      *
      *     FMNO:
      *          FD-REC CONTAINS 1 FORM DESCRIPTOR
      *                 UP TO 999 RECORDS (FMNO 1-999)              
      *
      *     DCODE, DBNO:
      *          FD-REC1 CONTAINS DATA BASE FOR 2 FILES      
      *                  100 FIELDS PER RECORD                       
      *                  FD-DCODE IS ALWAYS 99, FD-DBNO 1-99         
      * 
      * REV:                                            
      *  010697 BAH  ADDED FD-PROG-PREFIX TO ALLOW MORE CUSTOM
      *              FORM PROGRAMS. IF PREFIX = 0, THEN PROGRAM
      *              WILL BE PRT0(FD-PROG), OTHERWISE IT WILL
      *              STILL USE LONP(FD-PROG)
      *  011797 JTG  ADDED FD-FORM-CHG-CD TO ALLOW FOR AUTO ASSESS
      *              OF FORM CHARGES TO LOAN RECORD LN-OTHBAL
      *  112097 JTG  EXPANDED RECORD BY 30 BYTES
      *  121797 KEC  ADDED FD-FXPRESS FOR FORMSXPRESS FORMS.
      *  121897 KEC  REMOVED FD-FXPRESS FOR FORMSXPRESS FORMS.
      *              ADDED NEW PREFIX TO FD-PROG-PREFIX: 'X'.
      *  123097 KEC  ADDED FD-FXPRESS FOR FORMSXPRESS FORMS (AGAIN).
      *               FORMSXPRESS OPTION CAN BE FLAGGED ON OR OFF IN
      *               ORDER TO PRINT TO A STANDARD DOT MATRIX PRINTER OR
      *               TO A LASER PRINTER. THE FORM-PROGRAM NAME
      *               CONFIGURATION (LONP/PRT0) WILL BE MAINTAIN, THAT 
      *               IS WHY FORMSXPRESS NEEDS ITS OWN FLAG.
      *  020498 KEC  ADDED NEW PREFIX TO FD-PROG-PREFIX: 'X'.
      * KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  990722 BAH RENAMED FD-FXPRESS TO FD-SMARTPORT, BF MOTOR
      *             ALL SMARTPORT NEEDS IS A TRIGGER ON THE FIRST LINE
      *             PRINT OF A DOCUMENT, SO WE TEST THIS FLAG AND HARD
      *             CODE THE PROGRAM NAME TO PRINT ON LINE 1 OF DOCUMENT.
      *             LOOK AT LONPMP FOR AN EXAMPLE. BF MOTOR CREDIT PR# 917
      * KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 2000.0301 ADDED FD-CUSTOMER-STATMENT
      * KEC 2002.1015 {PR#00000} PARADATA <A15> [JIM ROSENAUER ]
      *               CHANGED "FD-PROG-PREFIX" TO ACCEPT "Z" (LASER).
      * KEC 2002.1024 {PR#00000} PARADATA <A15> [JIM ROSENAUER ]
      *               CHANGED "FD-PROG-PREFIX" TO ACCEPT 1 THRU 7(LASER)
      *
      * KEC 2005.0804 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               ADDED "FD-OPTION" WHICH IS REDEFINED FROM THE SAME
      *               BYTE POSITION OF FD-SMARTPORT.
      *               ADDED 88 LEVELS FOR "FD-PROG-PREFIX" TO LOOK AT
      *               'LONP', 'PRT' AND 'PCL' (PCL IS NEW).
      * BAH 050921 ADDED FD-VALID-BRANCH FOR LENDMARK PR# 2579
      *            TO RESTRICT REQUEST OF SPECIFIC FORM LETTERS
      * BAH 090805 ADDED FD-MIN-FORM-SENT-DAYS TO ALLOW RESTRICTIONS OF
      *            NOT ALLOWING ANOTHER FORM OF SAME TYPE (C OR S) TO BE
      *            SENT UNLESS 'X' # DAYS HAS PASSED, REGACC PR# 251
      * BAH 091001 ADDED FD-SKIP-BAD-ADR TO PREVENT LETTERS SENT IF PRIMARY 
      *            MAKER HAS THE BAD ADDRESS FIELD "Y", REGACC PR# 256
      *  CS 110228 ADDED FD-SKIP-BAD-ADR "M"  MODERN  PR#3720
      *
      * KEC 2011.1003 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               ADDED 88 LEVEL FOR "FD-PROG-PREFIX" TO LOOK AT
      *               'PST' (POSTSCRIPT).
      * BLM 120725 ADDED FD-SECURITY-PROFILE, HIGHEST LETTER OF USER 
      *            LOGIN ALLOWED TO REQUEST FORM, REGACC PR# 316 
      * BAH 130325 ADDED FD-TRAILER-REC TO READ FTFILE, TRAILER, AND
      *            CHECK IF STATE ALLOWED TO REQUEST FORM, REGACC PR# 335
      ******************************************************************
      
      *---------------------------------------------------------
      *    FORM DESCRIPTOR RECORD   (FD-REC)
      *---------------------------------------------------------
       01  FD-REC.
           03  FD1-KEY.
               05  FD-FMNO             PIC 9(3).
           03  FD-DESC                 PIC X(25).

      *=================================================================
      *
      *    F D - O P T I O N
      *
      *-----------------------------------------------------------------
      *    FD-OPTION IS TO DEFINE THE SPECIAL TYPE OF FORM (FM-REC) IS.
      *
      *    "C" = CUSTOMER STATEMENT {CUSTMT}
      *    "E" = EXTRACTION (CORPORATE)
      *    "N" = STANDARD  FORM {LONP OR PRT}
      *    "P" = PCL/LASER FORM {PCL} (PARADATA)
      *    "T" = PST/LASER FORM {PST} (PARADATA)
      *    "Y" = SMARTPORT
      *
      *    *NOTE* FD-OPTION = FD-SMARTPORT & FD-CUSTOMER-STATMENT
      *
      *=================================================================
           03  FD-OPTION               PIC X(1).
               88  FD-OPTION-CUSTMT        VALUE "C".
               88  FD-OPTION-EXTRACTION    VALUE "E".
               88  FD-OPTION-STANDARD      VALUE "N" " ".
               88  FD-OPTION-PCL-LASER     VALUE "P".
               88  FD-OPTION-PST-LASER     VALUE "T".
               88  FD-OPTION-SMARTPORT     VALUE "Y".
               88  FD-OPTION-VALID         VALUE "C" "E" "N"
                                                 "P" "T" "Y".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SMARTPORT IS AN OUTSIDE PACKAGE USED BY BF MOTOR CREDIT THAT
      * NEEDS A TRIGGER ON LINE 1 OF OUR DOCUMENTS, THEN THEY DO ALL
      * THE MOVING OF FIELDS TO THE CORRECT POSITIONS.
      * IF A PRINT PROGRAM ALREADY EXISTS (ANOTHER CUSTOMER USING),
      * ALL YOU HAVE TO DO IS ADD A TEST ON THIS FLAG, AND IF "Y",
      * HARD CODE THE FIRST LINE OF PRINT TO BE THE PROGRAM NAME.
      * LOOK AT LONPMP AS AN EXAMPLE.
      * IF A NEW PROGRAM IS NEEDED, ALL WE HAVE TO DO IS PRINT
      * EVERY FIELD NEEDED IN ORDER, BUT EACH ON A SEPARATE LINE
      * ALWAYS STARTING IN COLUMN 1. WE DONT HAVE TO DO ANY LAYOUTS OR
      * LINE MEASURING !!  BETH AND KEVIN 7/23/99
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  FD-SMARTPORT            REDEFINES FD-OPTION
                                       PIC X(1).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    THIS IS FOR CREATE A CUSTOMER STATEMENT THROUGH A EXTERNAL
      *     VENDOR.  TO USE, MUST FLAG IT "C".
      * YOU CAN USE "MC" AS THE PROGRAM NAME IN DOC DESIGN, IT DOESNT MATTER
      * LP/CUSTMT IS THE PROGRAM THAT USES THIS FIELD AND IT DOES NOT
      * CREATE FORM REQUESTS (FMFILE) WHEN THIS FLAG IS "C", IT CREATES
      * THE CSFILE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  FD-CUSTOMER-STATMENT    REDEFINES FD-OPTION
                                       PIC X(1).

      *=================================================================
      * FD-FORM-CHG-CD: WHEN OTHER THAN A " ", POINTS TO THE
      *                 CD-FILE (AF-REC) WHICH IDENTIFIES A
      *                 FEE TO BE AUTOMATICALLY ADDED TO THE
      *                 LOANS LN-OTHBAL FIELD
      *-----------------------------------------------------------------
           03  FD-FORM-CHG-CD          PIC X.

      *=================================================================
      *
      *    F D - P R O G - P R E F I X
      *
      *-----------------------------------------------------------------
      * FD-PROG-PREFIX: NEED TO ALLOW A SPACE TO GET ENTERED
      *                 IN FDMAIN, BUT THATS NOT A VALID
      *                 PROGRAM TO CALL
      *
      *-----------------------------------------------------------------
      *   PREFIX   PROGRAM
      *   ------   -------
      *     " "    LONP(FD-PROG)
      *
      *     "0"    PCL0(FD-PROG) DOC DESIGN LETTERS (PCLPMC)
      *            PST0(FD-PROG) DOC DESIGN LETTERS (PSTPMC)
      *            PRT0(FD-PROG) *ALL* FORM TYPES {CL/AH/PP/MISC/CHECKS}
      *
      *     "1"    PCL1(FD-PROG) AH/CL - ACCIDENT & HEALTH / CREDIT LIFE
      *            PST1(FD-PROG) AH/CL - ACCIDENT & HEALTH / CREDIT LIFE
      *            PRT1(FD-PROG) *ALL* FORM TYPES {CL/AH/PP/MISC/CHECKS}
      *
      *     "2"    PCL2(FD-PROG) AH/CL - ACCIDENT & HEALTH / CREDIT LIFE
      *            PST2(FD-PROG) AH/CL - ACCIDENT & HEALTH / CREDIT LIFE
      *            PRT2(FD-PROG) *ALL* FORM TYPES {CL/AH/PP/MISC/CHECKS}
      *
      *     "3"    PCL3(FD-PROG) PP  - PERSONAL PROPERTY
      *            PST3(FD-PROG) PP  - PERSONAL PROPERTY
      *            PRT3(FD-PROG) *ALL* FORM TYPES {CL/AH/PP/MISC/CHECKS}
      *
      *     "4"    PCL4(FD-PROG) IUI/VSI/OTHER INS.
      *            PST4(FD-PROG) IUI/VSI/OTHER INS.
      *            PRT4(FD-PROG) *ALL* FORM TYPES {CL/AH/PP/MISC/CHECKS}
      *
      *     "5"    PCL5(FD-PROG) MISC. FORMS
      *            PST5(FD-PROG) MISC. FORMS
      *                          - APPLICATIONS
      *                          - CERTIFICATES
      *                          - NOTICES
      *                          - TITLES
      *                          - UCC
      *                          - WAIVERS
      *            PRT5(FD-PROG) *ALL* FORM TYPES {CL/AH/PP/MISC/CHECKS}
      *
      *     "6"    PCL6(FD-PROG) CHECKS
      *            PST6(FD-PROG) CHECKS
      *            PRT6(FD-PROG) *ALL* FORM TYPES {CL/AH/PP/MISC/CHECKS}
      *
      *     "7"    PCL7(FD-PROG) CONTRACTS
      *            PST7(FD-PROG) CONTRACTS
      *                          - PROMISSORY NOTE
      *                          - RETAIL INSTALLMENT
      *                          - SECURITY AGREEMENT
      *                          - TRUTH IN LENDING DISCLOSURE
      *            PRT7(FD-PROG) *ALL* FORM TYPES {CL/AH/PP/MISC/CHECKS}
      *
      *     "8"    PCL8(FD-PROG) CONTRACTS
      *            PST8(FD-PROG) CONTRACTS
      *                          - PROMISSORY NOTE
      *                          - RETAIL INSTALLMENT
      *                          - SECURITY AGREEMENT
      *                          - TRUTH IN LENDING DISCLOSURE
      *            PRT8(FD-PROG) *ALL* FORM TYPES {CL/AH/PP/MISC/CHECKS}
      *
      *     "9"    PCL9(FD-PROG) CONTRACTS
      *            PST9(FD-PROG) CONTRACTS
      *                          - PROMISSORY NOTE
      *                          - RETAIL INSTALLMENT
      *                          - SECURITY AGREEMENT
      *                          - TRUTH IN LENDING DISCLOSURE
      *            PRT9(FD-PROG) *ALL* FORM TYPES {CL/AH/PP/MISC/CHECKS}
      *
      *   *NOTE* 'PCL#'IS FOR PARADATA'S LASER FORM SYSTEM.
      *   *NOTE* 'PCL#'IS FOR FD-OPTION-PCL-LASER.
      *   *NOTE* 'PST#'IS FOR PARADATA'S LASER FORM SYSTEM.
      *   *NOTE* 'PST#'IS FOR FD-OPTION-PCL-LASER.
      *   *NOTE* 'PRT#'IS FOR FD-OPTION-STANDARD.
      *
      *=================================================================
           03  FD-PROG-PREFIX          PIC X.
               88  FD-PROG-PREFIX-LONP       VALUE " ".
               88  FD-PROG-PREFIX-PCL        VALUE "0" "1" "2" "3" "4"
                                                   "5" "6" "7" "8" "9".
               88  FD-PROG-PREFIX-PST        VALUE "0" "1" "2" "3" "4"
                                                   "5" "6" "7" "8" "9".
               88  FD-PROG-PREFIX-PRT        VALUE "0" "1" "2" "3" "4"
                                                   "5" "6" "7" "8" "9".
               88  FD-PROG-PREFIX-VALID      VALUE " "
                                                   "0" "1" "2" "3" "4"
                                                   "5" "6" "7" "8" "9".

      *=================================================================
           03  FD-TYPE                 PIC X.

      *----------------------------------------------------------
      * FD-LNMEMO: IS USED TO CREATE A DATED LOAN MEMO IF SET
      *            "FORM 999 (XXXXXXXXXXXXXXXXXX) HAS BEEN PRINTED"
           03  FD-LNMEMO               PIC X.
      *----------------------------------------------------------
           03  FD-BODY                 PIC 9(4)  COMP.
           03  FD-TOP                  PIC 9(4)  COMP.
           03  FD-BOTTOM               PIC 9(4)  COMP.
           03  FD-PROG                 PIC XX.
           03  FD-PROMPTS              PIC X(80) OCCURS 30.
           03  FD-INFO.
               05  FD-FIELDINFO                  OCCURS 50.
                   07  FD-FIELDNO      PIC 9(4)  COMP.
                   07  FD-ROWCOL.
                       09  FD-FIELDROW PIC 9(2)  COMP.
                       09  FD-FIELDCOL PIC 9(2)  COMP.
                   07  FD-FIELDED      PIC 9(2)  COMP.
                   07  FD-FIELDKEY     PIC 9(4)  COMP.
      *----------------------------------------------------------
      * FD-1STFILE CODES:
      *    1 - LN (ONLY USE AS 1STFILE, IF YOU DONT NEED ANY NOTICE FIELDS)
      *    2 - BW (LABELS, 1098S, ONLY INFO FROM BORROWER FILE NEEDED)
      *    4 - NOTICE FILE (IF YOU NEED ANYTHING FROM IT, IT MUST BE 1STFILE)
      *
           03  FD-1STFILE              PIC 9(2)  COMP.
      *----------------------------------------------------------
      * FD-INCR-DELCNT: USED TO DETERMINE WHETHER TO INCREMENT
      *                 LN-DELCNT WHEN THIS FORM IS PRINTED
           03  FD-INCR-DELCNT          PIC X.
      *----------------------------------------------------------
      * FMTYPE:
      *    " " - STANDARD FORM (LONP/PRT0)
      *    "C" - COLLECTION FORM
      *          USED FOR SETTING LN-LASTCFORMNO, LN-LASTCFORMDATE
      *          ALLOWS FOR MAINTAINING 1 COLLECTION REQUEST MAX:
      *    "S" - SOLICITATION FORM
      *          USED FOR SETTING LN-LASTSFORMNO, LN-LASTSFORMDATE
           03  FD-FMTYPE               PIC X.
      *----------------------------------------------------------
           03  FD-PRCONT               PIC X.
           03  FD-FMLINK.
               05  FD-NEXTFM           PIC 9(3).
           03  FD-VALID-BRANCH         PIC X(4).
           03  FD-MIN-FORM-SENT-DAYS   PIC 99.
           03  FD-SKIP-BAD-ADR         PIC X.
           03  FD-SECURITY-PROFILE     PIC X.
           03  FD-TRAILER-REC          PIC X.
           03  FILLER                  PIC X(21).
 
      *---------------------------------------------------------
      *    DATA BASE RECORD   (FD-REC1)
      *---------------------------------------------------------
      *    DATA BASE RECORD   (FD-REC1)
      * VALID DATA BASE RECORDS:  (DBNO)
      *         1 - LN      6 - DL
      *         2 - BW      7 - COLLATERAL TRAILER (LT)
      *         3 - BR      8 - AUTO INSURANCE TRAILER (LT)
      *         4 - NO      9 - GB
      *         5 - LP     10 - LS (REPO TRAILER)
      *                    11 - LA (LONG APP)
      *                    12 - BA (BORROWER APP)
      *                    13 - MA (MINI APP)
      *
      *        21 - NT     23 - NC
      *        22 - IV
      *---------------------------------------------------------
       01  FD-REC1.
           03  FD2-KEY.
               05  FD-DCODE            PIC 99    COMP.
               05  FD-DBNO             PIC 99.
           03  FD-DATABASEX.
               05  FD-DATABASE         OCCURS 200.
                   07  FD-DNAME        PIC X(8).
                   07  FD-DOFFSET      PIC 9(4)  COMP.
                   07  FD-DDIGITS      PIC 9(2)  COMP.
                   07  FD-DTYPE        PIC 9(2)  COMP.
                   07  FD-DDECIMAL     PIC 9(2)  COMP.
           03  FD-FIELD-DESCS         REDEFINES FD-DATABASEX.
               05  FD-DATA-RECS        OCCURS 2
                                       PIC X(1300).
           03  FD-CALLKEY              OCCURS 2.
               05  FD-CALLID           PIC X(2).
               05  FD-KEYLOC           PIC 9(4)  COMP.
           03  FILLER                  PIC X(217).
