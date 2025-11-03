      * COPYMEMBER: LIBGL/GL01JH
      **********************************************************************
      *  GENERAL LEDGER JOURNAL ENTRY HEADER FILE
      *
      *  KEY 1 -  DATE / ID/ SEQUENCE
      *
      *  BLV 020410 SPLIT INTO FD & 01 COPY MEMBERS, ADDED JH-SIZE,
      *             UNCOMPED DISTRIBUTION-AMT, ADDED JH-USERID,
      *             JH-POSTDATE, JH-POSTTIME, INCREASED JH-UNUSED
      *  BLV 021024 REMOVE 2ND HALF OF JH-DESC, ONE FIELD OF 75 CHARACTERS
      *             SHOULD BE ENOUGH, INCREASE UNUSED, AND WILL CREATE
      *             NEW FILE (MH) TO BE MASTER TEMPLATE, THIS FILE WILL JUST
      *             CONTAIN DATED JOURNAL ENTRIES & HISTORY
      * GLB 030730  ADD SEQUENCE TO JH1-KEY
      * GLB 030730  MODIFY JH-DATE FROM 6 TO 8 DIGITS YYYYMMDD
      * GLB 030731  CHANGE JH-FLAG TO JH-GL-FLAG
      * BLV 031010  REMOVED JH-DISTRIBUTION-AMT, ADDED JH-WR-DATE
      **********************************************************************
       78  JH-SIZE                    VALUE 154.
       01  JH-REC.
           03  JH1-KEY.
               05  JH-DATE             PIC 9(8).
               05  FILLER REDEFINES JH-DATE.
                   07  JH-YYYY         PIC 9(4).
                   07  JH-MM           PIC 9(2).
                   07  JH-DD           PIC 9(2).
               05  JH-ID               PIC X(8).
               05  JH-SEQ              PIC 9(2).
           03  JH-TYPE                 PIC X(2).
           03  JH-ORDER                PIC X(2).
           03  JH-DESC                 PIC X(32).
           03  JH-EXPL                 PIC X(32).
           03  JH-GL-FLAG              PIC X.
           03  JH-USERID               PIC X(8).
           03  JH-POSTDATE             PIC 9(6)   COMP-6.
           03  JH-POSTTIME             PIC 9(6)   COMP-6.
      * JR-WR-DATE IS YYYYMM
           03  JH-WR-DATE              PIC 9(6)   COMP-6.
           03  FILLER                  PIC X(50).
