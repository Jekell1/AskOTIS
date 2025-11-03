      * COPYMEMBER: LIBWI/CISCAN.CMD
      ******************************************************************
      *
      *    COPY MEMBER: LIBWI/CISCAN.CMD
      *
      *    DESCRIPTION: HEADERS, LEGENDS AND MESSAGES FOR SCAN WINDOW
      *
      *=================================================================
      * REV:
      * KEC 2018-0823 LOAN APP MODULE REMOVAL
      *
      ******************************************************************
      *
      *    H E A D E R S
      *
      *=================================================================
       01  HEADERS-FOR-PRIMARY-KEYS.
           03  HEADER-LNAME      PIC X(16)     VALUE "LAST NAME       ".
           03  HEADER-REPCODE    PIC X(16)     VALUE "REPRESENTATIVE  ".
           03  HEADER-SSNO       PIC X(16)     VALUE "SOC. SEC. NUMBER".

      *=================================================================
      *
      *    L E G E N D S
      *
      *=================================================================
       01  LEGEND-BEG-DATE.
           03  FILLER PIC X(30) VALUE "F1-Reset, F3-ALL, F7-Exit, Ent".
           03  FILLER PIC X(30) VALUE "er Beginning Date            :".

       01  LEGEND-END-DATE.
           03  FILLER PIC X(30) VALUE "F1-Reset, F7-Exit, Enter Endin".
           03  FILLER PIC X(30) VALUE "g Date                       :".

       01  LEGEND-EQUIFAX.
           03  FILLER PIC X(30) VALUE "F1-Reset, F7-Exit, Enter 'Y' o".
           03  FILLER PIC X(30) VALUE "r 'N' for Equifax Inquiry    :".

       01  LEGEND-EXPERIAN.
           03  FILLER PIC X(30) VALUE "F1-Reset, F7-Exit, Enter 'Y' o".
           03  FILLER PIC X(30) VALUE "r 'N' for Experian Inquiry   :".

       01  LEGEND-OTHER.
           03  FILLER PIC X(30) VALUE "F1-Reset, F7-Exit, Enter 'Y' o".
           03  FILLER PIC X(30) VALUE "r 'N' for Other Inquiries    :".

       01  LEGEND-PRIMARY-LNAME.
           03  FILLER PIC X(30) VALUE "F1-Reset, F5-Toggle Scan Key, ".
           03  FILLER PIC X(30) VALUE "F7-Exit, Enter Last Name     :".

       01  LEGEND-PRIMARY-SSNO.
           03  FILLER PIC X(30) VALUE "F1-Reset, F5-Toggle Scan Key, ".
           03  FILLER PIC X(30) VALUE "F7-Exit, Enter Soc. Sec. Num.:".

       01  LEGEND-SELECTION.
           03  FILLER PIC X(30) VALUE "F1-Reset, F7-Exit, Enter on Cr".
           03  FILLER PIC X(30) VALUE "edit Bureau Inquiry Selection:".

       01  LEGEND-TRANSUNION.
           03  FILLER PIC X(30) VALUE "F1-Reset, F7-Exit, Enter 'Y' o".
           03  FILLER PIC X(30) VALUE "r 'N' for Transunion Inquiry :".

      *=================================================================
      *
      *    M E S S A G E S
      *
      *=================================================================
