      * COPYMEMBER: LIBLP/LPFDTRI
      **************************************************
      *             TR-CODE  =  "I" 
      *                INSURANCE CLAIMS
      *     THESE ARE MEMO ONLY, FOR REPORTING PURPOSES ONLY
      *
      * REV:
      *  BAH 040204 NEW FOR WORLD INSURANCE CLAIMS UPDATE
      *  BLV 040621 ADD TI-CLAIM-NUMBER
      *  BLF 050111 ADD TI-INSCOMP, WORLD PR# 366
      *  BLM 090318 ADD TI-DAYS-PAID TI-PMT-TYPE, TI-FROM-DATE, 
      *             TI-TO-DATE, WORLD PR# 631
      **************************************************
       01  TI-REC.
           03  TI-KEY.
               05  TI-DAY              PIC 99       COMP.
               05  TI-BRNO             PIC 9(4)     COMP.
               05  TI-CODE             PIC X.
               05  TI-SEQ              PIC 9(4)     COMP.
               05  TI-FILLER           PIC X(8).
      *-------------------------------------------------
           03  TI-CLAIMS-REC.
               05  TI-WAIS                 PIC X(4).
               05  FILLER                  PIC X(4).
               05  TI-TYPE                 PIC 9(3).
                   88 TI-TYPE-CL           VALUE 510.
                   88 TI-TYPE-AH           VALUE 511.
                   88 TI-TYPE-PP           VALUE 527.
                   88 TI-TYPE-AUTO         VALUE 528.
                   88 TI-TYPE-BK-NONF      VALUE 536.
                   88 TI-TYPE-NONF         VALUE 538.
                   88 TI-TYPE-UNEMP        VALUE 454.
                   88 TI-TYPE-AD-D         VALUE 459.
                   88 TI-TYPE-L-PROT       VALUE 470.
                   88 TI-TYPE-ROADG        VALUE 463.
                   88 TI-TYPE-VALID        VALUE 454 459 463 470 510
                                                 511 527 528 536 538.
               05  TI-AMOUNT               PIC S9(8)V99.
               05  TI-AMT                  REDEFINES TI-AMOUNT.
                    07 TI-AMT-BYTE         PIC X OCCURS 10.
               05  TI-CLAIM-NUMBER         PIC X(9).
               05  FILLER                  PIC X(13).
               05  TI-LNAME                PIC X(15).
               05  TI-FNAME                PIC X(10).
               05  TI-DATE                 PIC 9(6).
               05  TI-DATEX REDEFINES TI-DATE.
                   07  TI-YR               PIC 99.
                   07  TI-MO               PIC 99.
                   07  TI-DA               PIC 99.
               05  TI-DRAFT-ALP.
                   07  TI-DRAFT            PIC 9(8).
               05  FILLER                  PIC X(40).
               05  TI-PRODUCER             PIC 9(6).
               05  TI-PRODUCER-ALP REDEFINES TI-PRODUCER.
                   07  TI-PRODUCER1        PIC X.
                   07  TI-NEW-BRNO         PIC 9(5).
 
           03  TI-USERID               PIC X(8).
           03  TI-POSTTIME             PIC 9(6)   COMP-6.
           03  TI-POSTDATE             PIC 9(6)   COMP-6.
           03  TI-INSCOMP              PIC 9(2)   COMP-6.
           03  TI-DAYS-PAID            PIC 9(6)   COMP-6.
           03  TI-PMT-TYPE             PIC X(10).
           03  TI-FROM-DATE            PIC 9(6)   COMP-6.
           03  TI-TO-DATE              PIC 9(6)   COMP-6.
           03  FILLER                  PIC X(374).
