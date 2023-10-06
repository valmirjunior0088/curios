defmodule Curios.Scope.Variable do
  @moduledoc """
  Free and bound variables.

  Clients of this module should only ever interact with free variables,
  as bound variables are managed internally by `Curios.Scope`.
  """

  import Curios.Macros

  defmodule Free do
    @moduledoc """
    Free variables.
    """

    deftypedstruct([],
      name: String.t()
    )
  end

  defmodule Bound do
    @moduledoc """
    Bound variables.
    """

    deftypedstruct([],
      index: non_neg_integer(),
      qualifier: non_neg_integer()
    )
  end

  @type t() :: Free.t() | Bound.t()

  @spec new(String.t()) :: t()
  def new(name) do
    %Free{name: name}
  end

  defmodule BoundError do
    @moduledoc """
    Error used when an unexpected bound variable is encountered. Bound variables
    are meant to be used only internally in `Curios.Scope`.
    """

    deftypedexception([],
      index: non_neg_integer(),
      qualifier: non_neg_integer()
    )

    @spec exception(index: non_neg_integer(), qualifier: non_neg_integer()) :: t()
    def exception(index: index, qualifier: qualifier) do
      %__MODULE__{
        index: index,
        qualifier: qualifier
      }
    end

    @spec message(t()) :: String.t()
    def message(%__MODULE__{index: index, qualifier: qualifier}) do
      "bound variable `##{index}[#{qualifier}]` -- should not happen"
    end
  end

  @spec name(t()) :: String.t()
  def name(variable) do
    case variable do
      %Free{name: name} ->
        name

      %Bound{index: index, qualifier: qualifier} ->
        raise BoundError, index: index, qualifier: qualifier
    end
  end
end
