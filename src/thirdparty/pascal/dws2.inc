{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at                                          }
{                                                                      }
{    http://www.mozilla.org/MPL/                                       }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    The Original Code is DelphiWebScriptII source code, released      }
{    January 1, 2001                                                   }
{                                                                      }
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. Portions created by Matthias Ackermann are             }
{    Copyright (C) 2000 Matthias Ackermann, Switzerland. All           }
{    Rights Reserved.                                                  }
{                                                                      }
{    Contributor(s): Willibald Krenn                                   }
{                                                                      }
{**********************************************************************}

// DWSII include file

{global compiler options}

{$A+} // align on
{$B-} // BoolEval off
{$X+} // extended syntax
{$H+} // long strings

{$IFNDEF DWS_DEFAULT_OPTIMIZATION}

{if you want to debug DWSII, then turn the next compilerswitch OFF}
{$O+} // optimization default: O+

{$IFOPT O+} // <<< build for release if optimization is turned on
  {$C-} // Assertions off
  {$Q-} // no overflow checking
  {$R-} // no range checking
  {$W-} // no frames
  {$WARNINGS OFF} // no warnings.
{$ELSE}     // <<< build for debug if optimization is turned off
  {$C+} // Assertions on
  {$Q+} // overflow checking
  {$R+} // range checking
  {$W+} // frames
  {$WARNINGS ON} // warnings for debugging
{$ENDIF}

{$ENDIF}

{$T-} // typed addr off - otherwise you'll get problems with ComSupport
      // <<.CreateResFmt(@SNoMethod, [Names]);>>


{$IFDEF LINUX}              // Kylix 1 = VER140!
  {$DEFINE NEWVARIANTS}     // new Delphi variant code
  {$DEFINE NEWDESIGN}       // new in K1/D6
  {$DEFINE VER140UP}
{$ELSE}
  {$IFDEF MSWINDOWS}
     {$DEFINE NEWVARIANTS}
     {$DEFINE NEWDESIGN}
     {$DEFINE DELPHI6up}
     {$DEFINE VER140UP}
  {$ENDIF}
{$ENDIF}


