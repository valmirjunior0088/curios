defmodule Curios.Example do
  @moduledoc """
  Example of a syntax tree that `Curios.Scope` can manage.
  Based on the untyped lambda calculus.
  """

  import Curios.Macros

  alias Curios.{
    Scope.Variable,
    Scope.Visitor.Accept,
    Scope.Visitor,
    Scope
  }

  defmodule Var do
    @moduledoc """
    Variables.
    """

    deftypedstruct([],
      variable: Variable.t()
    )

    defimpl Accept do
      alias Curios.Example

      @spec accept(@for.t(), Visitor.t(Example.t())) :: Example.t()
      def accept(%@for{variable: variable}, visitor) do
        Visitor.apply(variable, visitor)
      end
    end
  end

  defmodule Abs do
    @moduledoc """
    Abstractions (i.e. functions).
    """

    deftypedstruct([],
      output: Scope.t(Example.t())
    )

    defimpl Accept do
      alias Curios.Example

      @spec accept(@for.t(), Visitor.t(Example.t())) :: Example.t()
      def accept(%@for{output: output}, visitor) do
        %@for{output: Accept.accept(output, visitor)}
      end
    end
  end

  defmodule App do
    @moduledoc """
    Applications (i.e. function calls).
    """

    deftypedstruct([],
      function: Example.t(),
      argument: Example.t()
    )

    defimpl Accept do
      alias Curios.Example

      @spec accept(@for.t(), Visitor.t(Example.t())) :: Example.t()
      def accept(%@for{function: function, argument: argument}, visitor) do
        %@for{
          function: Accept.accept(function, visitor),
          argument: Accept.accept(argument, visitor)
        }
      end
    end
  end

  @type t() :: Var.t() | Abs.t() | App.t()

  @spec var(String.t()) :: t()
  def var(name) do
    %Var{variable: Variable.new(name)}
  end

  @spec abs([String.t()], t()) :: t()
  def abs(targets, body) do
    %Abs{output: Scope.construct(fn variable -> %Var{variable: variable} end, targets, body)}
  end

  @spec app(t(), t()) :: t()
  def app(function, argument) do
    %App{function: function, argument: argument}
  end

  @spec eval(t()) :: t()
  def eval(example) do
    case example do
      %Var{variable: variable} ->
        %Var{variable: variable}

      %Abs{output: output} ->
        %Abs{output: output}

      %App{function: function, argument: argument} ->
        case eval(function) do
          %Abs{output: output} -> eval(Scope.eliminate(output, [argument]))
          function -> %App{function: function, argument: argument}
        end
    end
  end
end
