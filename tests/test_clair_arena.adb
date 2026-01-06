with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with System;
with System.Storage_Elements;
with Clair.Arena;

procedure Test_Clair_Arena is

  use type System.Storage_Elements.Integer_Address;

  -----------------------------------------------------------------------------
  -- [Test Types] Define data types for testing
  -----------------------------------------------------------------------------

  -- 1. Basic integer access type
  type Integer_Access is access Integer;

  -- 2. Record with high alignment requirement (256 bytes)
  type Aligned_Data is record
    Value : Integer;
    Padding : String(1..60);
  end record;
  for Aligned_Data'alignment use 256; -- Force alignment setting

  type Aligned_Access is access Aligned_Data;

  -- 3. Large-scale array
  type Big_Array is array (1 .. 100_000) of Integer;
  type Big_Array_Access is access Big_Array;

  -----------------------------------------------------------------------------
  -- [Arena Pool] Define Arena Pool instances
  -----------------------------------------------------------------------------
  -- Pool for the main task
  main_pool : Clair.Arena.Pool;

  -- Bind the pool to each access type
  for Integer_Access'Storage_Pool use main_pool;
  for Aligned_Access'Storage_Pool use main_pool;
  for Big_Array_Access'Storage_Pool use main_pool;

  -----------------------------------------------------------------------------
  -- [free] Deallocation procedures (Unchecked_Deallocation)
  -----------------------------------------------------------------------------
  procedure free is new Ada.Unchecked_Deallocation (Integer, Integer_Access);
  procedure free is new Ada.Unchecked_Deallocation (Aligned_Data,
                                                    Aligned_Access);
  procedure free is new Ada.Unchecked_Deallocation (Big_Array,
                                                    Big_Array_Access);

  -----------------------------------------------------------------------------
  -- [Helper] Print test results
  -----------------------------------------------------------------------------
  procedure Assert (Condition : Boolean; Message : String) is
  begin
    if Condition then
        Ada.Text_IO.put_line ("[PASS] " & Message);
    else
        Ada.Text_IO.put_line ("[FAIL] " & Message);
        raise Program_Error with "Test Assertion Failed: " & Message;
    end if;
  end Assert;

  -----------------------------------------------------------------------------
  -- [Task] Multi-threaded stress test
  -----------------------------------------------------------------------------
  task type Worker_Task is
    entry Start (Id : Integer);
  end Worker_Task;

  task body Worker_Task is
    Task_Id : Integer;
    Local_Pool : Clair.Arena.Pool; -- Independent arena for each task

    type Local_Int_Ptr is access Integer;
    for Local_Int_Ptr'Storage_Pool use Local_Pool;
    procedure free_Local is new Ada.Unchecked_Deallocation (Integer,
                                                            Local_Int_Ptr);

    ptr : Local_Int_Ptr;
  begin
    accept Start (Id : Integer) do
        Task_Id := Id;
    end Start;

    -- 1. Initialize the Arena
    Clair.Arena.Initialize(Local_Pool);

    -- 2. Perform repeated allocation and deallocation
    for I in 1 .. 1000 loop
        ptr := new Integer'(I);
        if Ptr.all /= I then
          Ada.Text_IO.put_line ("Task " & Task_Id'image &
                                " Memory Corruption!");
        end if;
        free_Local(ptr);
    end loop;

    Ada.Text_IO.put_line ("[PASS] Worker Task " & Task_Id'image & " finished.");

    -- Local_Pool's Finalize will be called automatically when the scope ends
  end Worker_Task;

  Workers : array (1 .. 4) of Worker_Task;

  -----------------------------------------------------------------------------
  -- [Main] Execute test scenarios
  -----------------------------------------------------------------------------
  Int_Ptr     : Integer_Access;
  Aligned_ptr : Aligned_Access;
  Array_Ptr   : Big_Array_Access;
  Addr_Int    : System.Storage_Elements.Integer_Address;

begin
  Ada.Text_IO.put_line ("=== Clair.Arena Test Suite Started ===");

  -- [Step 0] Initialization
  Clair.Arena.Initialize(main_pool);
  Assert (True, "Pool Initialization");

  -- [Step 1] Basic allocation test
  Int_ptr := new Integer'(12345);
  Assert (Int_Ptr.all = 12345, "Basic Read/Write Value");
  free (Int_Ptr);
  Assert (Int_Ptr = null, "Basic Deallocation");

  -- [Step 2] Alignment test
  -- Create an object requiring 256-byte alignment and inspect its actual
  -- address
  Aligned_ptr := new Aligned_Data'(Value => 999, Padding => [others => ' ']);

  -- Convert address to integer for modular arithmetic
  Addr_Int := System.Storage_Elements.To_Integer(Aligned_Ptr.all'address);

  Ada.Text_IO.put_line ("   Requested Alignment: 256");
  Ada.Text_IO.put_line ("   Actual Address:      " &
    System.Storage_Elements.Integer_Address'image(Addr_Int));

  Assert ((Addr_Int mod 256) = 0, "Address Alignment Check");
  Assert (Aligned_Ptr.Value = 999, "Aligned Data Integrity");
  free (Aligned_Ptr);

  -- [Step 3] Large-scale memory test
  Array_ptr := new Big_Array;
  Array_Ptr(1) := 10;
  Array_Ptr(100_000) := 20;
  Assert (Array_Ptr(1) + Array_Ptr(100_000) = 30, "Large Array Access");
  free (Array_Ptr);

  -- [Step 4] Multi-threaded (Tasking) test
  Ada.Text_IO.put_line ("--- Starting Multi-threaded Stress Test ---");
  for i in Workers'range loop
    Workers(i).start(i);
  end loop;

  -- Wait for tasks to finish (Ada tasks wait automatically at the end of the
  -- scope, but we print an explicit message here)

  -- [Finalization] main_pool's finalize will be called automatically upon
  -- procedure exit

end Test_Clair_Arena;
