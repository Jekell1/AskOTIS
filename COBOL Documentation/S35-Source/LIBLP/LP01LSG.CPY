      * COPYMEMBER: LIBLP/LP01LSG
      ******************************************************************
      *    LOAN STATUS INFORMATION FILE : 
      *
      *      GPS STARTER DEVICE
      *
      * LSG1-KEY - BRNO, ACCTNO, SEQNO
      *
      * REV:
      *  BAH 110125 ADDED "G" GPS STARTER DEVICE FOR PCCREDIT PEOPLE PR# 3699
      *  BAH 140407 ADDED LS-G-GPS-INSTALLER FOR PEOPLES CREDIT, PR# 4515
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      *   CS 190516 REMOVE ENDING FILLER, NOT IS USE AT WORLD
      *  BAH 20230600 ADDED FILLER
      ******************************************************************
       78  LSG-SIZE                       VALUE 330.
       01  LSG-REC.
           03  LSG1-KEY.
               05  LSG-BRNO               PIC 9(4).
               05  LSG-ACCTNO             PIC 9(8).
               05  LSG-SEQNO              PIC 99.

      $XFD DATE=YYYYMMDD
           03  LSG-LTOUCH-DATE              PIC 9(8)     COMP-3.

           03  LSG-GPS-AREA.
               05  LSG-SERIAL-NO             PIC X(12).
               05  LSG-STARTER-STAT          PIC X.
               05  LSG-WARNING-STAT          PIC X.
               05  LSG-RECOVERY-STAT         PIC X.
      $XFD DATE=YYYYMMDD
               05  LSG-LAST-COMMAND-DATE     PIC 9(8)      COMP-3.
               05  LSG-LAST-ADDRESS          PIC X(75).
      $XFD DATE=YYYYMMDD
               05  LSG-LAST-AUTO-REPORT-DATE PIC 9(8)      COMP-3.
               05  LSG-GPS-INSTALLER         PIC X(3).
               05  FILLER                    PIC X(208).
