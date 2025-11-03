      * COPYMEMBER: LIBSP/SP01BB
      **************************************************************************
      * BULLETIN BOARD FILE
      *
      *
      *  BB1-KEY = BB-CATAGORY, BB-SUBCATAGORY, BB-USERID, BB-SEQNO
      *  BB2-KEY = BB-CATAGORY, BB-SUBCATAGORY, BB-NAME
      *
      * REV:
      * BAH 190219 REMOVED BB-MANAGER-LOCATION REDEFINES, NOT NEEDED
      * KEC 190312 CHANGED 'BB-MEMO PIC X(600)' TO BE SIMPLE GROUP
      *            LEVEL NAMED BB-MEMO-TABLE THEN DEFINED
      *            'BB-MEMO PIC X(60) OCCURS 10' AS ITS ONLY FIELD.
      *            THEN CHANGED THE GROUP LEVEL REDEFINES OF 
      *            BB-MEMOX TO USE BB-MEMO-TABLE.
      *  CS 190515 REMOVED ENDING FILLER
      * BAH 2020.0610 ADDED BB-LTOUCH-DATE
      * BAH 20230612 ADDED FILLER
      **************************************************************************
       78  BB-SIZE                    VALUE  1000.
       01  BB-REC.
           03  BB1-KEY.
               05  BB-CATAGORY        PIC X(04).
               05  BB-SUBCATAGORY     PIC X(04).
               05  BB-USERID          PIC X(10).
               05  BB-SEQNO           PIC 9(06).
      *    -------------------------------------------------------------
           03  BB-NAME                PIC X(20).
           03  BB-LOGNAME             PIC X(10).
           03  BB-POSTNAME            PIC X(10).
           03  BB-OTHERBR             PIC X(10).
      $XFD DATE=YYYYMMDD
           03  BB-POSTDATE            PIC 9(08)       COMP-3.
           03  BB-POSTTIME            PIC 9(06)       COMP-3.
           03  BB-PASSWORD            PIC X(08).
      *    -------------------------------------------------------------
      * 1200
           03  BB-MESSAGE-AREA.
               05  BB-GROUP           PIC X(04).
               05  BB-BRNO            PIC 9(4)    COMP-3.
               05  BB-ACTION          PIC X(40).
               05  BB-MEMO-TABLE.
                   07 BB-MEMO-LINE    PIC X(60) OCCURS 10.
      $XFD DATE=YYYYMMDD
           03  BB-LTOUCH-DATE         PIC 9(8) COMP-3.
           03  FILLER                 PIC X(257).
