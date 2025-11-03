      * COPYMEMBER: LIBLP/LP01DT
      **********************************************************************
      *          (DEALER TRANSACTION FILE RECORD)    BYTES = 250
      *    KEY 1 - BRNO,DLNO, CLASS, ACCOUNT #, DATE, SEQUENCE #
      *    KEY 2 - BATCHID, BATCHID-SEQ
      *    KEY 3 - STATUS + KEY 1
      *    KEY 4 - CHECK NO
      *
      * REV :
      *  JTG 072696 ADDED DT-BATCHID-SEQ
      *             REMOVED DT-BRNO FROM DT1-KEY
      *             ADDED DT2-KEY, DT3-KEY, DT4-KEY
      *             CHANGED DT-SEQ FROM 9(4) TO 9(3)
      *  JTG 103196 CHANGED DT4-KEY FROM DT-DATE TO DT-CKNO
      *  JTG 112097 EXPANDED BY 18 BYTES
      *  MJD 990126 ----- UPDATED IN FLEXGEN -----
      * KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * GLB 2000.0726 ADD PAYMENT BUCKETS FOR RSV AMTS
      *  CS 2006.0320 ADD BUSINESS EVENT CODE [SAL]
      *  CS 060515 CHANGED DT-EVENT-CD TO X(2) FROM 9(2).
      * BLF 071217 ADDED DT-PART-HELD-STATUS, REGACC PR# 185
      * BAH 160614 ADDED FILLER, EXPANDED DATES, CLASS, SEQNO, MOVED
      *            BRNO UNDER DT1-KEY
      **********************************************************************
       78  DT-SIZE                          VALUE 250.
       01  DT-REC.
           03  DT3-KEY.
               05  DT-STATUS                PIC X.
                   88  DT-STATUS-UNRELEASED  VALUE " ".
                   88  DT-STATUS-RELEASED    VALUE "R".
                   88  DT-STATUS-PRINTED     VALUE "P".
               05  DT1-KEY.
                   07  DT-BRNO              PIC 9(4).
                   07  DT-NUM               PIC 9(6).
                   07  FILLER       REDEFINES DT-NUM.
                       09  DT-DL            PIC 9(4).
                       09  DT-DLBR          PIC 99.
                   07  DT-DLNO      REDEFINES DT-NUM
                                            PIC 9(4)V99.
                   07  DT-CLASS             PIC 9(3).
                   07  DT-ACCTNO            PIC 9(8).
      *>>XDD DATE=YYYYMMDD
                   07  DT-DATE              PIC 9(8).
                   07  DT-SEQ               PIC 9(3).
      **********************************************************
      *>>XDD DATE=YYYYMMDD
           03  DT-LTOUCH-DATE           PIC 9(8)       COMP-3.
           03  DT-TYPE                  PIC X.
               88  DT-TYPE-PURCH         VALUE "P".
               88  DT-TYPE-SERV          VALUE "S".
           03  DT-LPSEQNO               PIC 9(5)       COMP-3.
           03  DT-TRCD                  PIC XX.
               88  DT-TRCD-BALFWRD       VALUE "BF".
               88  DT-TRCD-NEW           VALUE "NW".
               88  DT-TRCD-TRANS         VALUE "TX".
      **********************************************************
           03  DT-DLRVPRIN              OCCURS 3
                                        PIC S9(7)V99   COMP-3.
           03  DT-DLRVCHGS              OCCURS 3
                                        PIC S9(7)V99   COMP-3.
           03  DT-DLPARVHELD            PIC S9(7)V99   COMP-3.
           03  DT-DLPARVPAID            PIC S9(7)V99   COMP-3.
           03  DT-PROCEEDS              PIC S9(7)V99   COMP-3.
      *****************************************************
           03  DT-RELEASED              PIC S9(7)V99   COMP-3.
           03  DT-PAYMENTS              PIC S9(7)V99   COMP-3.
           03  DT4-KEY.
               05  DT-CKNO              PIC 9(6)       COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  DT-CKDATE                PIC 9(8)       COMP-3.
           03  DT2-KEY.
               05  DT-BATCHID           PIC 9(10)      COMP-3.
               05  DT-BATCHID-SEQ       PIC 9(5)       COMP-3.
           03  DT-PAID                  PIC X.
               88  DT-PAID-BATCH         VALUE "B".
               88  DT-PAID-SYSTEM        VALUE "S".
               88  DT-PAID-MANUAL        VALUE "M".
           03  DT-REVERSE               PIC X.
               88  DT-REVERSE-DELETED    VALUE "Y".
           03  DT-REVERSE-TRCD          PIC XX.
           03  DT-PURBAL                PIC S9(7)V99    COMP-3.
           03  DT-APCUR                 PIC S9(7)V99    COMP-3.
           03  DT-DLRVNO                PIC 9.
           03  DT-SELECTED              PIC X.
           03  DT-P4P5-TRCD             PIC XX.
      *  GLB 07/26/00
           03  DT-DLRVPRIN-PAY          PIC S9(7)V99  COMP-3.
           03  DT-DLRVCHGS-PAY          PIC S9(7)V99  COMP-3.
           03  DT-DLPARVHELD-PAY        PIC S9(7)V99  COMP-3.
           03  DT-CHECK-VOID            PIC X.
           03  DT-UNPAID                PIC X.
           03  DT-MEMO                  PIC X.
           03  DT-EVENT-CD              PIC X(2).
           03  DT-PART-HELD-STATUS      PIC X.
           03  FILLER                   PIC X(94).
