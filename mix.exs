defmodule Mapsd.Mixfile do
  use Mix.Project

  def project do
    [app: :mapsd,
     version: "0.1.0",
     language: :erlang,
     description: description,
     package: package,
     deps: deps]
  end

  def application do
    []
  end

  defp deps do
    []
  end

  defp description do
    "Erlang Maps Dict API"
  end

  defp package do
    [files: ~w(src rebar.config README.markdown LICENSE),
     maintainers: ["Michael Truog"],
     licenses: ["BSD"],
     links: %{"GitHub" => "https://github.com/okeuday/mapsd"}]
   end
end
