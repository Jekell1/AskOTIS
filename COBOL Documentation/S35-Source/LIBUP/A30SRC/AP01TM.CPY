      * COPYMEMBER: LIBAP/AP01TM
      ******************************************************************
      *
      * IDENTIFICATION: LIBAP/AP01TM
      *
      * DESCRIPTION   : TERMS MASTER FILE
      *
      *=================================================================
      * REV:
      * 10/08/2015 BAH ADDED TM-KEY TO MAKE FILE INDEXED, PD0005
      * 10/22/2015 BAH INCREASED FILLER, PD0003
      * 09/16/2016 KEC ADDED
      ******************************************************************
       78  TM-SIZE                             VALUE 150.
       01  TM-REC.
           03  TM-KEY                          PIC 99.
           03  TM-LTOUCH-DATE                  PIC 9(08)      COMP-3.
           03  TM-NUMBER                       PIC 99.
           03  TM-DESC                         PIC X(25).
           03  TM-DISCOUNT                     PIC 99V99      COMP-3.
           03  TM-DISCDAYS                     PIC 9(4)       COMP-3.
           03  TM-NETDAYS                      PIC 9(4)       COMP-3.
           03  TM-MONTH                        PIC 99         COMP-3.
           03  TM-CUT-OFF                      PIC 99         COMP-3.
           03  TM-NO-PAYS                      PIC 99         COMP-3.
           03  TM-DEFAULT-FLAG                 PIC X.
           03  TM-UNUSED                       PIC X(100).
