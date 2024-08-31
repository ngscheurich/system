package cmd

import (
	"fmt"
	"os"
	"strings"

	"github.com/pelletier/go-toml/v2"
	"github.com/spf13/cobra"
)

type ProgManifest struct {
	Entries []ProgManifestEntry
}

type ProgManifestEntry struct {
	Name string
	Path string
	Dest string
}

func GetProgManifest() ProgManifest {
	var cfg ProgManifest
	doc := readProgManifest()
	err := toml.Unmarshal([]byte(doc), &cfg)
	if err != nil {
		panic(err)
	}
	return cfg
}

func readProgManifest() []byte {
	path := fmt.Sprintf("%s/config/entries.toml", SystemDir())
	data, err := os.ReadFile(path)
	if err != nil {
		panic(err)
	}
	return data
}

func GetEntryFsDest(e ProgManifestEntry) string {
	dest := e.Dest
	parts := strings.Split(dest, "$")
	if len(parts) == 2 {
		dest = os.Getenv(parts[1])
	}
	return fmt.Sprintf("%s/%s", dest, e.Path)
}

var configCmd = &cobra.Command{
	Use:   "config",
	Short: "Manage program configurations",
	Run:   func(cmd *cobra.Command, args []string) {},
}

func init() {
	rootCmd.AddCommand(configCmd)
}
