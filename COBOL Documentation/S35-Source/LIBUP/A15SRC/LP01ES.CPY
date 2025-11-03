      * COPYMEMBER: LIBLP/LP01ES
      **************************************************************************
      *
      *   IDENTIFICATION: LIBLP/LP01ES
      *
      *   DESCRIPTION   : LOAN ESCROW TRAILER
      *
      *-----------------------------------------------------------------
      *   KEY 1 -  ACCTNO, SEQNO
      *
      *-----------------------------------------------------------------
      * REV:
      *   CS 151104 ADD ES-DISBURSED-CNT-TAXES  COLTN PR#4867
      *  BLM 160107 ADD ES-INS-TABLE, COLTN PR#4835             
      ******************************************************************
       78  ES-SIZE                                         VALUE 1088.
      *-----------------------------------------------------------------
       01  ES-REC.
           03  ES1-KEY.
               05  ES-ACCTNO                PIC 9(8).

           03  ES-ESCROW-SCHEDULE           OCCURS 40.
               05  ES-SCHD-EFF-DATE         PIC 9(6)       COMP-6.
               05  ES-SCHD-1STPYDATE        PIC 9(6)       COMP-6.
               05  ES-SCHD-ESCROW           PIC S9(7)V99   COMP-3.
               05  ES-SCHD-TERM             PIC 9(3)       COMP-6.
               05  ES-SCHD-COLLECTED        PIC S9(7)V99   COMP-3.
      * 729
           03  ES-BALANCE                   PIC S9(7)V99   COMP-3.
      *-------------------------------------------------------------------------
      *  ESCROW STAT TABLE:  SUB 1 = YTD
      *                      SUB 2 = LAST YTD
      *                      SUB 3 = LOAN TO DATE
      *-------------------------------------------------------------------------
      * 734, 769, 804
           03  ES-STAT-TABLE OCCURS 3.
               05  ES-PREPAID              PIC S9(7)V99   COMP-3.
               05  ES-COLLECTED            PIC S9(7)V99   COMP-3.
               05  ES-DISBURSED-TAXES      PIC S9(7)V99   COMP-3.
               05  ES-DISBURSED-INS        PIC S9(7)V99   COMP-3.
               05  ES-DISBURSED-PMI        PIC S9(7)V99   COMP-3.
               05  ES-DISBURSED-OTHER      PIC S9(7)V99   COMP-3.
               05  ES-DISBURSED-REF        PIC S9(7)V99   COMP-3.
      * 839
           03  ES-DISBURSED-CNT-TAXES      PIC S9(7)V99   COMP-3 
                                                       OCCURS 3.
      *-------------------------------------------------------------------------
      *  ESCROW MISC TABLE:  SUB 1 = HOMEOWNERS   SUB 2 = COUNTY TAX
      *                      SUB 3 = CITY TAX     SUB 2 = OTHER INS
      *                      SUB 4 = UCC          SUB 6 = OTHER
      *-------------------------------------------------------------------------
           03  ES-MISC-TABLE OCCURS 6.
               05  ES-MISC-AMT             PIC S9(7)V99   COMP-3.
               05  ES-EXP-DATE             PIC 9(6)       COMP-3.
           03  FILLER                      PIC X(181).


