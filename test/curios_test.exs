defmodule Curios.Test do
  @moduledoc """
  Tests for `Curios`.
  """

  use ExUnit.Case
  doctest Curios

  alias Curios.Example

  test "identity function works" do
    identity = Example.abs(["x"], Example.var("x"))

    application =
      identity
      |> Example.app(identity)
      |> Example.app(identity)
      |> Example.app(identity)
      |> Example.app(Example.var("y"))

    assert Example.eval(application) == Example.var("y")
  end

  test "const function works" do
    const = Example.abs(["a"], Example.abs(["b"], Example.var("a")))

    application =
      const
      |> Example.app(Example.var("x"))
      |> Example.app(Example.var("y"))

    assert Example.eval(application) == Example.var("x")
  end
end
