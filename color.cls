 
 /*------------------------------------------------------------------------
    File        : color
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Varshitha.R
    Created     : Tue May 10 14:23:49 IST 2022
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS businesslogic.color: 
    {ui/itrequest.i}.

    DEFINE PUBLIC PROPERTY iusrTicket AS INTEGER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    CONSTRUCTOR PUBLIC color (  ):
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL getcolor(INPUT iuserTicket AS INTEGER):
       // METHOD PUBLIC LOGICAL getcolororder( INPUT ordernum as INTEGER ):
      //METHOD PUBLIC logical ordercolor( INPUT ordernum as INTEGER ):
        DEFINE VARIABLE result1 AS LOGICAL NO-UNDO.
        DEFINE VARIABLE objcolor as businesslogic.ItRequest.
        objcolor = new businesslogic.ItRequest().
        objcolor:iusrTicketId = iuserTicket.
        objcolor:userTicketfetch(INPUT TEMP-TABLE ttuserTicket:DEFAULT-BUFFER-HANDLE).

        FIND FIRST ttuserTicket WHERE ttuserTicket.tticketStatus = "Hold" NO-LOCK NO-ERROR.
        IF AVAILABLE ttuserTicket THEN
        DO:
            result1 = true.
        END.
        else
        DO:
            result1 = false.
        END.

        //RETURN lcolor.
        

        RETURN result1.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    DESTRUCTOR PUBLIC color ( ):

    END DESTRUCTOR.

END CLASS.