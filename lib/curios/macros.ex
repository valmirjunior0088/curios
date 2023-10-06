defmodule Curios.Macros do
  @moduledoc """
  Macros used in the project.
  """

  @spec deftypedstruct([Macro.t()], [{Atom.t(), Macro.t()}]) :: Macro.t()
  defmacro deftypedstruct(variables, fields) do
    keys = Keyword.keys(fields)

    quote do
      @enforce_keys unquote(keys)

      defstruct unquote(keys)

      @type t(unquote_splicing(variables)) :: %__MODULE__{
              unquote_splicing(fields)
            }
    end
  end

  @spec deftypedexception([Macro.t()], [{Atom.t(), Macro.t()}]) :: Macro.t()
  defmacro deftypedexception(variables, fields) do
    keys = Keyword.keys(fields)

    quote do
      @enforce_keys unquote(keys)

      defexception unquote(keys)

      @type t(unquote_splicing(variables)) :: %__MODULE__{
              unquote_splicing(fields)
            }
    end
  end
end
