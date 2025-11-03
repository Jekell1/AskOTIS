      * COPYMEMBER: LIBLP/LP01ST
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/LP01ST.CPY
      *
      * DESCRIPTION   :  STATE CODE FILE
      *
      * ST1-KEY = ST-CODE
      *
      *=================================================================
      * REV:
      * 081124 BAH ADDED ST-REINSTATE-WINDOW FOR REGACC PR# 228
      * 090127 BAH ADDED ST-REPO-RENOTICE FOR REGACC PR# 236
      * 120905 BAH ADDED ST-TITLE-RELEASE-TYPE FOR REGACC PR# 320
      * 140421 BAH ADDED ST-AUCTION-DATE-REQ FOR REGACC PR# 352
      *
      * KEC 151022 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *     INCREASE FILE SIZE; ADDED ST-SIZE VALUE 300;
      *     MERGED SU-FILE <A15> INTO ST-FILE.
      *     ADDED ST-LTOUCH-DATE.
      * 170719 BAH ADDED ST-MAX-LOAN-AVAIL WORLD PR#1260
      * 180913 BAH ADDED ST-MAX-TAX-LOAN-AMT AND ST-NSF-FEE-AMT, #1334
      * 190218 BAH REMOVED ST-GEOGRAPHIC-CODEX REDEFINES, NOT USED
      * 190510  CS ADD FUTURE FIELDS
      * 190517 KEC INCREASED SOME FUTURE FIELDS QUANITIES
      * BAH 20200324 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * BAH 20210427 REMOVED FUTURE FIELDS
      *  BAH 20230600 ADDED FILLER
      ******************************************************************
       78  ST-SIZE                                VALUE 245.
       01  ST-REC.
           03  ST1-KEY.
               05  ST-CODE                PIC X(02).
      $XFD DATE=YYYYMMDD
           03  ST-LTOUCH-DATE             PIC 9(08)   COMP-3.
           03  ST-DESC                    PIC X(16).
           03  ST-REINSTATE-WINDOW        PIC X(01).
           03  ST-REPO-RENOTICE           PIC X(01).
           03  ST-TITLE-RELEASE-TYPE      PIC X(01).
           03  ST-AUCTION-DATE-REQ        PIC X(01).
           03  ST-GEOGRAPHIC-CODE         PIC 9(04).
           03  ST-MAX-LOAN-AVAIL          PIC 9(6)V99  COMP-3.
           03  ST-MAX-TAX-LOAN-AMT        PIC S9(7)V99 COMP-3.
           03  ST-NSF-FEE-AMT             PIC S9(5)V99 COMP-3.
           03  FILLER                 PIC X(200).
