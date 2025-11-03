      * COPYMEMBER: LIBLA/LA01SI
      ******************************************************************
      *                                                        RSZ=1000
      *  IDENTIFICATION:  LIBLA/LA01SI        (SI = SPANISH INQUISITION)
      *
      *  DESCRIPTION   :  LOAN APPLICATION MASTER FILE
      *                   ***NOTE*** LINK TO THE MA-FILE (LIBLA/LA01MA)
      *
      *=================================================================
      *
      *  ***** SPECIAL NOTES *****
      *
      *  1.  SI-REC WILL HAVE A MA-REC.
      *
      *=================================================================
      * REV:
      * KEC 2010.0701 {PR#00693} WORLD ACCEPTANCE <A15> [JIM ROSENAUER]
      *               ***NEW***
      *
      ******************************************************************
       78  SI-SIZE                                           VALUE 1000.
      *=================================================================
       01  SI-REC.

      *------------------------------------------------------{ KEY: #1 }
           03  SI1-KEY.
               05  SI-APPNO                      PIC 9(10).
               05  FILLER                        REDEFINES SI-APPNO.
                   07  SI-APP-NUMBER             PIC 9(08).
                   07  SI-APP-SEQNO              PIC 9(02).
                       88  SI-APP-SEQNO-PRIMARY      VALUE 1.
                       88  SI-APP-SEQNO-SECONDARY    VALUE 2 THRU 4.
                       88  SI-APP-SEQNO-VALID        VALUE 1 THRU 4.

      ******************************************************************
           03  SI-LNAM2                          PIC X(16).
           03  SI-LOCATION                       PIC X(24).
           03  SI-RFC-TAX-ID                     PIC X(12).
           03  SI-VOTER-REG-NUM                  PIC X(19).
           03  SI-COMMENT-01                     PIC X(50).
           03  SI-COMMENT-02                     PIC X(50).
           03  FILLER                            PIC X(819).
