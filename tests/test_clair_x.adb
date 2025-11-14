-- tests/test_clair_x.adb
with Ada.Text_IO;
with Ada.Exceptions;
with Clair.X;
with Clair.File;

procedure Test_Clair_X is
   use Ada.Text_IO;
   use Clair.X;

   Display : Display_Handle;
   FD      : Clair.File.Descriptor;
begin
   Put_Line ("===> Testing Clair.X bindings...");

   -- 시도: DISPLAY 환경변수 기반으로 디스플레이 열기
   begin
      Display := open_display;
      Put_Line ("Display opened successfully.");

      -- 연결 FD 확인
      FD := get_connection_fd (Display);
      Put_Line ("Connection FD: " & Integer'Image (Integer (FD)));

      -- 디스플레이 닫기
      close_display (Display);
      Put_Line ("Display closed successfully.");
   exception
      when Display_Open_Error =>
         Put_Line ("❌ Failed to open display (DISPLAY not set?)");
      when Display_Close_Error =>
         Put_Line ("❌ Failed to close display");
      when Connection_Fd_Error =>
         Put_Line ("❌ Invalid connection FD");
      when Invalid_Display_Handle =>
         Put_Line ("❌ Invalid display handle");
      when E : others =>
         Put_Line ("❌ Unexpected exception: " &
                   Ada.Exceptions.Exception_Name (E));
   end;

   Put_Line ("===> Clair.X test finished.");
end Test_Clair_X;
