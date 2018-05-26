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
{    Contributor(s): ______________________________________.           }
{                                                                      }
{**********************************************************************}

unit dws2CompStrings;

interface

resourcestring
  UNT_CircularReference = 'Circular reference to symbol "%s"';
  UNT_NameIsEmpty = 'Name property mustn''t be empty!';
  UNT_NameAlreadyExists = 'Name "%s" already exists!';
  UNT_UnitNameNotDefined = 'Property %s.UnitName is undefined!';
  UNT_ParameterNameAlreadyExists = 'Parameter name "%s" already exists';
  UNT_DependencyError = 'Dependency check from unit "%s" to unit "%s" failed: %s';

  UNT_UnitGenerationError = 'Tdws2Unit: "%s" -- %s';
  UNT_SymbolGenerationError = '%s: "%s" -- %s';

  UNT_DatatypeUnknown = 'DataType "%s" not found';
  UNT_AutoInstantiateWithoutClass =
    'AutoInstantiate is true but DataType "%s" isn''t a class';

  UNT_ClassAlreadyDefined = 'Class already defined';
  UNT_ClassNameAlreadyDefined =
    'The class "%s" is already used for another symbol %s';
  UNT_SuperClassUnknwon = 'Superclass "%s" not found';
  UNT_ReadAccessNotFound = 'ReadAccess "%s" not found';
  UNT_WriteAccessNotFound = 'WriteAccess "%s" not found';

  UNT_InvalidArrayBounds = 'LowBound is higher than HighBound';

  UNT_CantChangeUnitname =
    'Can''t change UnitName while property "Script" is assigned!';

  ADP_ChainIsFormingLoop = 'Adpater chain is forming a loop!';
  ADP_IncompatibleAdapters = 'Incompatible Adapters: %s -> %s';

implementation

end.
