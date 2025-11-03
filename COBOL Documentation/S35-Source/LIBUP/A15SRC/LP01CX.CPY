      * COPYMEMBER: LIBLP/LP01CX
      **************************************************
      *           ( TEMPORARY CHECK FILE )
      *    KEY 1 - BRANCH #, CHECK TYPE, SEQUENCE #
      *    KEY 2 - PAYEE NAME
      * REV:
      *  112596 BAH ADDED CX-RESCISSION, ADVANCE MTG (#401)
      *  112097 JTG EXPANDED BY 30 BYTES
      *  062598 BAH ADDED CX-TILLNO SO IT DOESNT USE CX-ACCTNO
      *             ANYMORE, TILLNO IS NOW ALPHA..
      * KEC 1999.0510 <<<<<<<< 'A60-TO-A15' CONVERSION UPDATED >>>>>>>>
      *  050323 BLF ADDED CX-CK-PAID-NEW TO TRACK TYPE OF
      *         REPLACEMENT CHECK
      *  060608 ADDED CX-EVENT-CD  PR#2503 SECURIT
      *  060712 BLF ADDED CX-PAYEE-NO, DEVELOPMENT PR# 2633
      *  061107 BAH ADDED 88 CX-UPDATE-RESCISSION TO IDENTIFY THAT THE LOAN
      *             WAS UPDATED OUT OF RESCISSION AND THESE CHECKS NEED TO
      *             BE PRINTED BEFORE THE DAY CLOSES ! REGWAN PL# 556
      **************************************************
       01  CX-REC.
           03  CX1-KEY.
               05  CX-BRNO         PIC 9(4)     COMP-3.
               05  CX-TYPE         PIC X(2).
               05  CX-SEQNO        PIC 9(4)     COMP-3.
           03  CX-AMT              PIC S9(7)V99 COMP-3.
           03  CX2-KEY.
               05  CX-PAYEE        PIC X(30).
           03  CX-ADR1             PIC X(24).
           03  CX-ADR2             PIC X(24).
           03  CX-CITY             PIC X(15).
           03  CX-STATE            PIC X(2).
           03  CX-ZIP              PIC X(10).
           03  CX-REF              PIC 9(6)     COMP-3.
           03  CX-REASON           PIC X(30).
           03  CX-GLDIST           OCCURS 5.
               05  CX-GLNO         PIC 9(11)    COMP-3.
               05  CX-GLAMT        PIC S9(7)V99 COMP-3.
           03  CX-FREE1            PIC X(70).
           03  CX-FREE2            PIC X(70).
           03  CX-VOID             PIC X.
           03  CX-CLASS            PIC 99       COMP-3.
           03  CX-CKNO             PIC 9(6)     COMP-3.
      *  GLB  07/26/00 REDEFINED
      *    03  CX-DLNO             PIC 9(6)     COMP-3.
           03  CX-DLNO             PIC 9(4)V99  COMP-3.
           03  CX-DATE             PIC 9(6)     COMP-3.
           03  CX-UNUSED           PIC 9(8)     COMP-3.
           03  CX-PAID             PIC X.
           03  CX-PGM              PIC XX.
           03  CX-BATCHID          PIC 9(10)    COMP-3.
           03  CX-BWPROC           PIC X.
      *  GLB  08/07/00 REPLACED CX-LN-NUMBER WITH CX-CKNO-NEW
           03  CX-CKNO-NEW         PIC 9(6)     COMP-3.
           03  CX-DLRVNO           PIC 9.
           03  CX-DT-SEQ           PIC 9(4)     COMP-3.
           03  CX-RESCISSION       PIC X.
               88  CX-RESCISSION-VALID          VALUE " " "Y".
               88  CX-IN-RESCISSION             VALUE "Y".
      * THIS IS WHEN THE CHECK WAS RELEASED FROM RESCISSION AND IS
      * READY TO PRINT. WE NEEDED TO IDENTIFY THEM IN LONPL0 FOR CLOSING
               88  CX-UPDATE-RESCISSION         VALUE "U".
           03  CX-TILLNO           PIC X.
      * GLB 08/09/00 UNCOMPED CX-ACCTNO, MOVED TO END
           03  CX-ACCTNO           PIC 9(8).
           03  CX-LN1-KEY REDEFINES CX-ACCTNO.
               05  CX-LN-NUMBER    PIC 9(6).
               05  CX-LN-SEQNO     PIC 9(2).
           03  CX-SERVACCT         PIC X.
           03  CX-DT-DATE          PIC 9(6).
           03  CX-SSNO             PIC 9(9) COMP-3.
           03  CX-DT-PAID          PIC X.
           03  CX-DT-UNPAID        PIC X.
           03  CX-CKPAID-NEW       PIC X.
           03  CX-EVENT-CD         PIC X(2).
           03  CX-PAYEE-NO         PIC 9(6) COMP-6.
           03  FILLER              PIC X(11).
