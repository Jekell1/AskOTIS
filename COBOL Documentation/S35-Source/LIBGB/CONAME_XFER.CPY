      ******************************************************************
      * LIBGB/CONAME_XFER.CPY
      *=================================================================
      * REV:
      * MJD 121017 ADDED "EXT-" PREFIX TO EXTERNAL CONAME FIELDS
      * MJD 121018 REPLACED CALL-EXFER WITH CALL-WRFORM.
      *
      * JKC 2020-1026 ADDED PARADATA-REL-VER-EXT AND PARADATA-REL-VERION
      *
      ******************************************************************
       CONAME-XFER SECTION.

      * DATE AND COMPANY NAME
           MOVE EXT-CONAME-NAM-DATA TO WR-BUF.
           MOVE NAM-LST TO WR-LIST.
           PERFORM CALL-WRFORM.

      * THE LOAN MANAGER FINANCE SYSTEM (A30)
           MOVE PARADATA-REL-VER-EXT TO PARADATA-REL-VERSION.
           MOVE PARADATA TO WR-BUF.
           MOVE PARA-LST TO WR-LIST.
           PERFORM CALL-WRFORM.

       CONAME-XFEREXIT.
           EXIT.
