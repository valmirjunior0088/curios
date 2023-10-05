defmodule Curios.Scope do
  @moduledoc """
  Generic lexical scoping mechanism for correct variable substitution in syntax trees with binding.
  """

  import Curios.Macros

  alias Curios.Scope.{
    Variable,
    Visitor.Accept,
    Visitor
  }

  deftypedstruct([a],
    inject: (Variable.t() -> a),
    content: a
  )

  @spec construct((Variable.t() -> a), [String.t()], a) :: t(a) when a: Accept.t()
  def construct(inject, targets, content) do
    apply = fn depth, variable ->
      case variable do
        %Variable.Free{name: name} ->
          case Enum.find_index(targets, &(&1 == name)) do
            nil -> inject.(variable)
            qualifier -> inject.(%Variable.Bound{index: depth, qualifier: qualifier})
          end

        %Variable.Bound{} ->
          inject.(variable)
      end
    end

    %__MODULE__{
      inject: inject,
      content: Accept.accept(content, Visitor.Private.new(apply))
    }
  end

  @spec eliminate(t(a), [a]) :: a when a: Accept.t()
  def eliminate(%__MODULE__{inject: inject, content: content}, sources) do
    apply = fn depth, variable ->
      case variable do
        %Variable.Free{} ->
          inject.(variable)

        %Variable.Bound{index: index, qualifier: qualifier} ->
          case depth == index do
            true -> Enum.fetch!(sources, qualifier)
            false -> inject.(variable)
          end
      end
    end

    Accept.accept(content, Visitor.Private.new(apply))
  end

  defimpl Accept do
    @spec accept(@for.t(a), Visitor.t(@for.t(a))) :: @for.t(a) when a: Accept.t()
    def accept(%@for{inject: inject, content: content}, visitor) do
      %@for{
        inject: inject,
        content: Accept.accept(content, Visitor.Private.descend(visitor))
      }
    end
  end
end
