defmodule Curios.Scope.Visitor do
  @moduledoc """
  An implementation of a variation of the visitor pattern for traversing a syntax tree.
  """

  import Curios.Macros

  alias Curios.Scope.Variable

  deftypedstruct([a],
    apply: (non_neg_integer(), Variable.t() -> a),
    depth: non_neg_integer()
  )

  defprotocol Accept do
    alias Curios.Scope.Visitor

    @spec accept(t(), Visitor.t(t())) :: t()
    def accept(t, visitor)
  end

  @spec apply(Variable.t(), t(a)) :: a when a: var
  def apply(variable, visitor) do
    visitor.apply.(visitor.depth, variable)
  end

  defmodule Private do
    @moduledoc """
    Private definitions for `Curios.Scope.Visitor`.
    """

    alias Curios.Scope.Visitor

    @spec new((non_neg_integer, Variable.t() -> a)) :: Visitor.t(a) when a: var
    def new(apply) do
      %Visitor{
        apply: apply,
        depth: 0
      }
    end

    @spec descend(Visitor.t(a)) :: Visitor.t(a) when a: var
    def descend(%Visitor{apply: apply, depth: depth}) do
      %Visitor{
        apply: apply,
        depth: depth + 1
      }
    end
  end
end
