module Infrastructure.Configuration.Providers.Domain.Yaml

open Microsoft.Extensions.Configuration

type Client = {
    Provider: IConfigurationRoot
}
type Connection = { Paths: string seq }
