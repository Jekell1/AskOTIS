      * COPYMEMBER: LIBGB/BRSECUREW
      ******************************************************************
      *
      *     BRSECUREW - WORKERS FOR BRSECURE - VERIFY SECURITY
      *                 FOR BRANCH RANGE/GROUP IN REPORTS
      *
      *=================================================================
      * REV:
      * MJD 021998 ADDED BRSECURE WORKERS FOR CALLING OF PASSWD.
      *            THIS WILL ALLOW PROGRAMS TO CHANGE ID AND SEQNO
      *            WITHOUT CHANGING BRSECURE COPY MEMBER.
      *
      * KEC 2015.1201 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED TO NOT USE BRSECURE-REC-DIR ANYMORE.
      *
      ******************************************************************
       01  BR-SECURITY-OKAY            PIC X.
       01  BRSECURE-REC-BUF.
           03  BRSECURE-REC-STAT             PIC XX      VALUE " ".
           03  BRSECURE-REC-KEY.
               05  BRSECURE-REC-PGM          PIC X(10)   VALUE "ALLBR".
               05  BRSECURE-REC-SEQ          PIC 99      VALUE 0.
           03  BRSECURE-REC-PROMPT1          PIC X(31)   VALUE
               "PLEASE ENTER THE PASSWORD ".
           03  BRSECURE-REC-PROMPT2          PIC X(31)   VALUE
               "TO REPORT ON THESE BRANCHES:".
