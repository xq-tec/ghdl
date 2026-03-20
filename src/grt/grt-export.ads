--  GHDL Run Time (GRT) - Design Export.
--  Copyright (C) 2026 xq-Tec GmbH
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <gnu.org/licenses>.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

with Interfaces; use Interfaces;

package Grt.Export is

   type Subscription_Index is new Unsigned_32;
   Invalid_Subscription_Index : constant Subscription_Index := Subscription_Index'Last;

   procedure Register_Design;

   --  Sets the Subscription field of a signal in the signal table.
   procedure Set_Signal_Subscription (Signal_Id, Element_Index : Unsigned_32;
                                      Subscription : Subscription_Index);
   pragma Export (C, Set_Signal_Subscription, "ghdl_set_signal_subscription");

   procedure Notify_Signal_Event (Sig_Idx : Subscription_Index; Value : Unsigned_64);

end Grt.Export;
